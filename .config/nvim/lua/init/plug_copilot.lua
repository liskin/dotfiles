local copilot = require'copilot'
local copilot_chat = require'CopilotChat'
local copilot_chat_actions = require'CopilotChat.actions'
local copilot_chat_cmp = require'CopilotChat.integrations.cmp'
local copilot_chat_fzflua = require'CopilotChat.integrations.fzflua'

copilot.setup {
	copilot_node_command = 'bwrap-node-copilot', -- restricted jail for the proprietary crap
	suggestion = {
		enabled = false,
	},
	panel = {
		keymap = {
			jump_next = "<M-]>",
			jump_prev = "<M-[>",
			open = "<M-CR>",
		},
	},
	filetypes = {
		["*"] = function()
			if vim.b.lsp_disabled or vim.g.lsp_disabled then
				return false
			end

			local name = vim.api.nvim_buf_get_name(0)
			local basename = vim.fs.basename(name)

			-- disable for .env files
			if string.match(basename, '^%.env.*') then
				return false
			end

			-- enable for everything else, should be okay considering I trigger this manually
			return true
		end,
	},
}

copilot_chat.setup {
	model = 'claude-3.5-sonnet',
	mappings = {
		complete = {
			insert = '',
		},
	},
}

copilot_chat_cmp.setup()

vim.keymap.set('n', '<Leader>cch', function()
	copilot_chat_fzflua.pick(copilot_chat_actions.help_actions())
end)
vim.keymap.set('n', '<Leader>ccp', function()
	copilot_chat_fzflua.pick(copilot_chat_actions.prompt_actions())
end)
