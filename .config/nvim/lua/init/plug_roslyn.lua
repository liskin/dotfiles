-- XXX: nvim-0.11 only
if vim.fn.has('nvim-0.11') == 0 then
	return
end

local fidget = require'fidget'

require'roslyn'.setup {}
vim.lsp.enable('roslyn', false)

local function run_tests_handler(err, result, ctx)
	if err then
		local client = vim.lsp.get_client_by_id(ctx.client_id)
		local client_name = client and client.name or string.format('client_id=%d', ctx.client_id)

		vim.notify(client_name .. ': ' .. tostring(err.code) .. ': ' .. err.message, vim.log.levels.ERROR)
		return
	end

	for _i, r in ipairs(result) do
		if vim.startswith(r.stage, "Running ") then
			fidget.notify(r.message, vim.log.levels.INFO, {
				group = "roslyn_tests",
				annote = r.stage,
				ttl = 5,
				hidden = not string.match(r.message, "= Summary ="),
			})
		end
	end
end

vim.lsp.config('roslyn', {
	-- roslyn.nvim assumes mason, reset cmd
	cmd = vim.lsp.config.roslyn_ls.cmd,

	commands = {
		["dotnet.test.run"] = function(command, ctx)
			local client = vim.lsp.get_client_by_id(ctx.client_id)
			if client then
				local progress = fidget.progress.handle.create({
					title = "Running tests…",
					message = "",
					lsp_client = client,
					percentage = 0,
				})
				client:request("textDocument/runTests", command.arguments[1], function(...)
					progress:finish()
					run_tests_handler(...)
				end, ctx.bufnr)
			end
		end,
	},
})
