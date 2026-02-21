local zen = require"zen-mode"

zen.setup {
	window = {
		width = 80,
		height = 0.9,
	},

	plugins = {
		diagnostics = {
			enabled = true,
		},
	},

	on_open = function(win)
		vim.cmd("Limelight")
		vim.wo[win].winhighlight = vim.wo[win].winhighlight .. ",Folded:ZenFolded"
	end,
	on_close = function()
		vim.cmd("Limelight!")
	end,
}

vim.keymap.set('n', '<Leader>zz', function() zen.toggle() end, { desc = "zen-mode toggle" })

---@diagnostic disable-next-line: duplicate-set-field
require"zen-mode.plugins".diagnostics = function(_state, disable)
	-- fix deprecation warnings
	vim.diagnostic.enable(not disable)
end

---@diagnostic disable-next-line: duplicate-set-field
require"zen-mode.view".on_win_enter = function()
	-- don't close on WinEnter into another window,
	-- it's annoying and sometimes segfaults nvim
end
