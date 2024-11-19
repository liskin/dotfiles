local copilot_chat = require'CopilotChat'
local copilot_chat_actions = require'CopilotChat.actions'
local copilot_chat_fzflua = require'CopilotChat.integrations.fzflua'
local copilot_chat_prompts = require'CopilotChat.prompts'

local function roast_my_code_system_prompt()
	local base = vim.split(copilot_chat_prompts.COPILOT_EXPLAIN, "\n")
	base[1] = "Explain it to me like you're a senior level brogrammer who thinks you're way better than you really are, and you're frustrated that you have to explain this to me."
	return table.concat(base, "\n")
end

copilot_chat.setup {
	model = 'claude-3.5-sonnet',
	chat_autocomplete = false,
	mappings = {
		complete = {
			insert = '<Plug>(tab_complete)',
		},
	},
	prompts = {
		GrammarCommit = {
			prompt = 'Improve grammar in the selected commit message.',
		},
		RoastMyCode = {
			prompt = 'Write an explanation for the selected code and diagnostics as paragraphs of text.',
			system_prompt = roast_my_code_system_prompt(),
		},
	},
}

vim.keymap.set('n', '<Leader>cc', function()
	copilot_chat_fzflua.pick(copilot_chat_actions.prompt_actions())
end)
