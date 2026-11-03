return {
	'CopilotChat.nvim',
	cmd = {
		'CopilotChat',
		'CopilotChatToggle',
	},
	keys = {
		{ '<Leader>CC', desc = "CopilotChat.nvim actions" },
	},
	after = function()
		require 'init.plug_copilotchat'
	end,
}
