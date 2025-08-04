local neotest = require'neotest'

---@diagnostic disable-next-line: missing-fields
neotest.setup {
	adapters = {
		-- require('neotest-vstest') {},
	},
	icons = {
		running = "…",
		running_animated = {"⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"}, -- {'⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'},

		passed = "✔",
		failed = "×",
		skipped = "–",
		unknown = "?",

		watching = "W",
		test = "T",

		notify = "",
	},
}

vim.keymap.set('n', '<Leader>tt', function() neotest.run.run(vim.fn.expand("%")) end, { desc = "Run File (Neotest)" })
vim.keymap.set('n', '<Leader>tT', function() neotest.run.run(vim.uv.cwd()) end, { desc = "Run All Test Files (Neotest)" })
vim.keymap.set('n', '<Leader>tr', function() neotest.run.run() end, { desc = "Run Nearest (Neotest)" })
vim.keymap.set('n', '<Leader>tl', function() neotest.run.run_last() end, { desc = "Run Last (Neotest)" })
vim.keymap.set('n', '<Leader>ts', function() neotest.summary.toggle() end, { desc = "Toggle Summary (Neotest)" })
vim.keymap.set('n', '<Leader>to', function() neotest.output.open({ enter = true, auto_close = true }) end, { desc = "Show Output (Neotest)" })
vim.keymap.set('n', '<Leader>tO', function() neotest.output_panel.toggle() end, { desc = "Toggle Output Panel (Neotest)" })
vim.keymap.set('n', '<Leader>tS', function() neotest.run.stop() end, { desc = "Stop (Neotest)" })
vim.keymap.set('n', '<Leader>tw', function() neotest.watch.toggle(vim.fn.expand("%")) end, { desc = "Toggle Watch (Neotest)" })
