-- see also 'init.lazy.plug_codecompanion'

local codecompanion = require'codecompanion'
local fidget = require'fidget'

codecompanion.setup {
	display = {
		chat = {
			show_header_separator = true,
			icons = {
				tool_success = "✔ ",
				tool_failure = "× ",
			},
		},
	},
	interactions = {
		inline = {
			adapter = {
				name = "copilot",
				model = "gpt-6-luna",
			},
		},
		chat = {
			adapter = {
				name = "copilot",
				model = "gpt-6-luna",
			},
		},
	},
}

do
	local spinners = {} -- one spinner per request

	vim.api.nvim_create_autocmd("User", {
		pattern = "CodeCompanionRequestStarted",
		callback = function(args)
			local req_id = args.data.id
			if spinners[req_id] then
				vim.notify("plug_codecompanion: spinner " .. tostring(req_id) .. " already exists", vim.log.levels.ERROR)
			else
				spinners[req_id] = fidget.progress.handle.create({
					title = "Thinking…",
					message = "",
					lsp_client = {
						name = "CodeCompanion(" .. args.data.adapter.formatted_name .. ")",
					},
					percentage = 0,
				})
			end
		end,
	})

	vim.api.nvim_create_autocmd("User", {
		pattern = "CodeCompanionRequestFinished",
		callback = function(args)
			local req_id = args.data.id
			if spinners[req_id] then
				spinners[req_id]:finish()
			else
				vim.notify("plug_codecompanion: spinner " .. tostring(req_id) .. " not exists", vim.log.levels.ERROR)
			end
		end,
	})
end

vim.keymap.set({'n', 'v'}, '<Leader>cc', "<cmd>CodeCompanionActions<cr>", {
	noremap = true,
	silent = true,
	desc = "codecompanion.nvim actions",
})
