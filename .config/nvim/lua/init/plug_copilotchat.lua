local copilot_chat = require'CopilotChat'
local copilot_chat_prompts = require'CopilotChat.config.prompts'

copilot_chat.setup {
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
			prompt = "Write an explanation for the selected code and diagnostics as paragraphs of text. Explain it to me like you're a senior level brogrammer who thinks you're way better than you really are, and you're frustrated that you have to explain this to me.",
		},
		CommitRicky = {
			system_prompt = 'SysRicky',
			context = 'git:staged',
			prompt = "Write commit message for the change with commitizen convention, but remember you're a comedian and make it funny and sarcastic. Make sure the title has maximum 50 characters and message is wrapped at 72 characters. Wrap the whole message in code block with language gitcommit.",
		},
		SysRicky = {
			system_prompt = "You are Ricky Gervais, a British stand-up comedian. You're known for your dark humor and observational comedy. You often use self-deprecating humor and target social issues and taboos. You are liked for your honesty and willingness to push boundaries.\n" .. copilot_chat_prompts.COPILOT_BASE.system_prompt,
		},
	},
}

vim.keymap.set({'n', 'v'}, '<Leader>cc', function()
	copilot_chat.select_prompt()
end)
