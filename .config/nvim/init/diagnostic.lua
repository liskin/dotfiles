vim.keymap.set('n', '<Plug>(lsp_prev)', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<Plug>(lsp_next)', vim.diagnostic.goto_next)
vim.keymap.set('n', '<Plug>(lsp_detail)', vim.diagnostic.open_float)

vim.diagnostic.config {
	severity_sort = true,
	virtual_text = {
		source = 'if_many',
		spacing = 10,
		-- virt_text_pos = 'right_align', -- needs nightly neovim
	},
}

-- hack for right alignment of virtual text
-- TODO: drop in favor of vim.diagnostic.config when possible
local orig_vim_api_nvim_buf_set_extmark = vim.api.nvim_buf_set_extmark
function vim.api.nvim_buf_set_extmark(buffer, ns_id, line, col, opts, ...)
	opts = opts or {}
	if col == 0 then
		opts.virt_text_pos = opts.virt_text_pos or 'right_align'
	end
	return orig_vim_api_nvim_buf_set_extmark(buffer, ns_id, line, col, opts, ...)
end
