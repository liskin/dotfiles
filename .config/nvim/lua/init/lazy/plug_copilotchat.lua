return {
	'CopilotChat.nvim',
	cmd = {
		'CopilotChat',
		'CopilotChatToggle',
	},
	keys = {
		{ '<Leader>cc', desc = "CopilotChat.nvim actions" },
	},
	after = function()
		require 'init.plug_copilotchat'
	end,
}
