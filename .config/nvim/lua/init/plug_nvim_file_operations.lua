require("nvim-file-operations").setup {
	auto_save = true,
}

vim.lsp.config('*', {
	capabilities = require("nvim-file-operations.config").default_capabilities(),
})
