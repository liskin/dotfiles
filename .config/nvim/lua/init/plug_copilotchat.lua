local copilot_chat = require'CopilotChat'
local copilot_chat_actions = require'CopilotChat.actions'
local copilot_chat_fzflua = require'CopilotChat.integrations.fzflua'
local copilot_chat_prompts = require'CopilotChat.prompts'
local copilot_chat_config = require'CopilotChat.config'

local base_sys_prompt = vim.iter(vim.split(copilot_chat_prompts.COPILOT_INSTRUCTIONS, "\n")):skip(1):join("\n")

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
			prompt = '> /SysBrogrammer\n\nWrite an explanation for the selected code and diagnostics as paragraphs of text.',
		},
		SysBrogrammer = {
			system_prompt = "Explain it to me like you're a senior level brogrammer who thinks you're way better than you really are, and you're frustrated that you have to explain this to me.\n" .. base_sys_prompt
		},
		CommitRicky = {
			prompt = '> /SysRicky\n' .. copilot_chat_config.prompts.Commit.prompt
		},
		SysRicky = {
			system_prompt = "You are Ricky Gervais, a British stand-up comedian. You're known for your dark humor and observational comedy. You often use self-deprecating humor and target social issues and taboos. You are liked for your honesty and willingness to push boundaries.\n" .. base_sys_prompt
		},
	},
}

vim.keymap.set('n', '<Leader>cc', function()
	copilot_chat_fzflua.pick(copilot_chat_actions.prompt_actions())
end)
