return {
	'neotest',
	keys = {
		{ '<Leader>tt', function() require'neotest'.run.run(vim.fn.expand("%")) end, desc = "Run File (Neotest)" },
		{ '<Leader>tT', function() require'neotest'.run.run(vim.uv.cwd()) end, desc = "Run All Test Files (Neotest)" },
		{ '<Leader>tr', function() require'neotest'.run.run() end, desc = "Run Nearest (Neotest)" },
		{ '<Leader>tl', function() require'neotest'.run.run_last() end, desc = "Run Last (Neotest)" },
		{ '<Leader>ts', function() require'neotest'.summary.toggle() end, desc = "Toggle Summary (Neotest)" },
		{ '<Leader>to', function() require'neotest'.output.open({ enter = true, auto_close = true }) end, desc = "Show Output (Neotest)" },
		{ '<Leader>tO', function() require'neotest'.output_panel.toggle() end, desc = "Toggle Output Panel (Neotest)" },
		{ '<Leader>tS', function() require'neotest'.run.stop() end, desc = "Stop (Neotest)" },
		{ '<Leader>tw', function() require'neotest'.watch.toggle(vim.fn.expand("%")) end, desc = "Toggle Watch (Neotest)" },
	},
	after = function()
		require 'init.plug_neotest'
	end,
}
