vim.cmd.source(vim.fn.stdpath("config") .. "/vimrc.vim")

require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"c",
		"lua",
		"query",
		"rust",
		"vim",
		"vimdoc",
	},
	highlight = {
		enable = true,
	},
}

vim.keymap.set('n', '<Plug>(lsp_prev)', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<Plug>(lsp_next)', vim.diagnostic.goto_next)

vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		local buf_local = { buffer = ev.buf }
		vim.keymap.set('n', '<Plug>(lsp_hover)', vim.lsp.buf.hover, buf_local)
		vim.keymap.set('n', '<Plug>(lsp_detail)', vim.diagnostic.open_float, buf_local)
		vim.keymap.set({'n', 'v'}, '<Plug>(lsp_code_action)', vim.lsp.buf.code_action, buf_local)
	end,
})

vim.diagnostic.config {
	severity_sort = true,
}
