lua <<END
vim.lsp.config('pylsp', {
	settings = {
		pylsp = {
			plugins = {
				-- enable mypy
				pylsp_mypy = { enabled = true },

				-- use ruff LSP instead for linting and formatting
				flake8 = { enabled = false },
				isort = { enabled = false },
			},
		},
	},
})

vim.lsp.enable('ruff')

require("conform").formatters_by_ft.python = { 'ruff_format', 'ruff_organize_imports' }
vim.g.ft_autoformat_python = true
END
