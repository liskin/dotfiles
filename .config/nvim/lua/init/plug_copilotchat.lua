local copilot_chat = require'CopilotChat'
local copilot_chat_actions = require'CopilotChat.actions'
local copilot_chat_fzflua = require'CopilotChat.integrations.fzflua'

copilot_chat.setup {
	model = 'claude-3.5-sonnet',
	chat_autocomplete = false,
	mappings = {
		complete = {
			insert = '<Plug>(tab_complete)',
		},
	},
}

vim.keymap.set('n', '<Leader>cc', function()
	copilot_chat_fzflua.pick(copilot_chat_actions.prompt_actions())
end)
