-- see also 'init.lazy.plug_neotest'

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
