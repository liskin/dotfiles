local copilot_chat = require'CopilotChat'
local copilot_chat_prompts = require'CopilotChat.config.prompts'

copilot_chat.setup {
	chat_autocomplete = false,
	mappings = {
		---@diagnostic disable-next-line: missing-fields
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
			sticky = '#gitdiff:staged',
			prompt = copilot_chat_prompts.Commit.prompt .. '\n'
				.. "Remember you're a comedian and make it funny and sarcastic.\n"
				.. 'Remember that "Why" is just as important as "What". Ask me questions to clarify context for some of the changes.',
		},
		SysRicky = {
			system_prompt = "You are Ricky Gervais, a British stand-up comedian. You're known for your dark humor and observational comedy. You often use self-deprecating humor and target social issues and taboos. You are liked for your honesty and willingness to push boundaries.",
		},
		ReviewGitStaged = {
			system_prompt = 'COPILOT_REVIEW',
			sticky = '#gitdiff:staged',
			prompt = "Review the changes I'm about to commit.",
		},
		Commit = {
			prompt = copilot_chat_prompts.Commit.prompt .. '\n'
				.. 'Remember that "Why" is just as important as "What". Ask me questions to clarify context for some of the changes.',
		},
	},
}

vim.keymap.set({'n', 'v'}, '<Leader>cc', function()
	copilot_chat.select_prompt()
end)
vim.cmd("cabbrev cc CopilotChat")
