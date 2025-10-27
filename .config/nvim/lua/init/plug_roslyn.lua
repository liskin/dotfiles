local fidget = require'fidget'

vim.cmd.runtime("plugin/roslyn.lua")
vim.lsp.enable('roslyn', false)

local function run_tests_handler(err, result, ctx)
	if err then
		local client = vim.lsp.get_client_by_id(ctx.client_id)
		local client_name = client and client.name or string.format('client_id=%d', ctx.client_id)

		vim.notify(client_name .. ': ' .. tostring(err.code) .. ': ' .. err.message, vim.log.levels.ERROR)
		return
	end

	fidget.notification.clear_history({
		group_key = "roslyn_tests",
	})
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

local progress_handles = {}
vim.api.nvim_create_autocmd("User", {
	pattern = "RoslynRestoreProgress",
	callback = function(ev)
		local client = vim.lsp.get_client_by_id(assert(ev.data.client_id))
		local token = assert(ev.data.params[1])
		local handle = progress_handles[token]
		if handle then
			handle:report({
				title = ev.data.params[2].state,
				message = ev.data.params[2].message,
			})
		else
			progress_handles[token] = fidget.progress.handle.create({
				title = ev.data.params[2].state,
				message = ev.data.params[2].message,
				lsp_client = client,
			})
		end
	end,
})
vim.api.nvim_create_autocmd("User", {
	pattern = "RoslynRestoreResult",
	callback = function(ev)
		local handle = progress_handles[ev.data.token]
		progress_handles[ev.data.token] = nil

		if handle then
			handle.message = ev.data.err and ev.data.err.message or "Restore completed"
			handle:finish()
		end
	end,
})

vim.lsp.config('roslyn', {
	settings = {
		['csharp|inlay_hints'] = {
			csharp_enable_inlay_hints_for_implicit_object_creation = true,
			csharp_enable_inlay_hints_for_implicit_variable_types = true,
			csharp_enable_inlay_hints_for_lambda_parameter_types = true,
			csharp_enable_inlay_hints_for_types = true,
			dotnet_enable_inlay_hints_for_indexer_parameters = true,
			dotnet_enable_inlay_hints_for_literal_parameters = true,
			dotnet_enable_inlay_hints_for_object_creation_parameters = true,
			dotnet_enable_inlay_hints_for_other_parameters = true,
			dotnet_enable_inlay_hints_for_parameters = true,
			dotnet_suppress_inlay_hints_for_parameters_that_differ_only_by_suffix = true,
			dotnet_suppress_inlay_hints_for_parameters_that_match_argument_name = true,
			dotnet_suppress_inlay_hints_for_parameters_that_match_method_intent = true,
		},
		['csharp|code_lens'] = {
			dotnet_enable_references_code_lens = false,
		},
	},

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
