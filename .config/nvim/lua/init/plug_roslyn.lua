-- XXX: nvim-0.11 only
if vim.fn.has('nvim-0.11') == 0 then
	return
end

require'roslyn'.setup {}
vim.lsp.enable('roslyn', false)

-- roslyn.nvim assumes mason, reset cmd
vim.lsp.config('roslyn', {
	cmd = vim.lsp.config.roslyn_ls.cmd,
})
