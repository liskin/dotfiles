vim.keymap.set('n', '<Plug>(lsp_prev)', function () vim.diagnostic.goto_prev({ float = false }) end)
vim.keymap.set('n', '<Plug>(lsp_next)', function () vim.diagnostic.goto_next({ float = false }) end)
vim.keymap.set('n', '<Plug>(lsp_detail)', vim.diagnostic.open_float)

vim.diagnostic.config {
	severity_sort = true,
	virtual_text = false,
}
