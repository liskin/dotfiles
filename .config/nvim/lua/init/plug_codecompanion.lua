local codecompanion = require'codecompanion'
local codecompanion_config = require'codecompanion.config'
local fidget = require'fidget'

codecompanion.setup {
	display = {
		chat = {
			show_header_separator = true,
			icons = {
				tool_success = "✔ ",
				tool_failure = "× ",
				buffer_pin = "📍 ",
				buffer_watch = "W ",
			},
		},
		icons = {
			loading = "… ",
			warning = "! ",
		},
	},
	strategies = {
		chat = {
			keymaps = {
				regenerate = { opts = { nowait = true } },
				codeblock = { opts = { nowait = true } },
			},
		},
		roles = {
			user = "User",
		},
	},
	prompt_library = {
		Commit = {
			strategy = "chat",
			description = "Generate a Commit Message",
			opts = {
				auto_submit = true,
				is_slash_cmd = true,
				short_name = "Commit",
			},
			prompts = {
				{
					role = codecompanion_config.config.constants.USER_ROLE,
					content = function()
						return string.format(
							"Write commit message for the change with commitizen convention. "
							.. "Keep the title under 50 characters and wrap message at 72 characters. "
							.. "Format as a gitcommit code block. "
							.. 'Remember that "Why" is just as important as "What". Ask me questions to clarify context for some of the changes. '
							.. "The change is the git diff listed below:\n"
							.. "```diff\n%s\n```\n",
							vim.fn.system("git diff --no-ext-diff --staged")
						)
					end,
					opts = {
						contains_code = true,
					},
				},
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

vim.keymap.set({'n', 'v'}, '<Leader>CC', "<cmd>CodeCompanionActions<cr>", { noremap = true, silent = true })
vim.cmd("cabbrev CC CodeCompanion")
