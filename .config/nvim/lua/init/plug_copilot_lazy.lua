vim.api.nvim_create_autocmd("SourcePost", {
	pattern = "*/plugin/copilot.lua",
	callback = function()
		require 'init.plug_copilot'
	end,
})
