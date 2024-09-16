require'lsp-format'.setup()

vim.api.nvim_create_user_command('LspFormat', function()
	vim.lsp.buf.format()
end, {})
