lua <<END
vim.lsp.enable('roslyn')
vim.g.lsp_autoformat_roslyn = true

require'neotest'.setup_project(vim.uv.cwd(), {
	adapters = {
		require('neotest-vstest') {
		},
	},
})
END
