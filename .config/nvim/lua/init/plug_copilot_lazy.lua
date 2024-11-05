vim.api.nvim_create_autocmd("SourcePost", {
	pattern = "*/plugin/copilot.lua",
	callback = function()
		vim.cmd.packadd('CopilotChat.nvim')
		require 'init.plug_copilot'
	end,
})
