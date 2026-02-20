local zen = require"zen-mode"

zen.setup {
	window = {
		width = 80,
		height = 0.9,
	},

	on_open = function(_win)
		vim.cmd("Limelight")
	end,
	on_close = function()
		vim.cmd("Limelight!")
	end,
}

vim.keymap.set('n', '<Leader>zz', function() zen.toggle() end, { desc = "zen-mode toggle" })
