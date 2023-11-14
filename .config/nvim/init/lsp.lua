vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		local buf_local = { buffer = ev.buf }
		vim.keymap.set('n', '<Plug>(lsp_hover)', vim.lsp.buf.hover, buf_local)
		vim.keymap.set('n', '<Plug>(lsp_detail)', vim.diagnostic.open_float, buf_local)
		vim.keymap.set({'n', 'v'}, '<Plug>(lsp_code_action)', vim.lsp.buf.code_action, buf_local)
	end,
})

-- rounded borders of LSP floating windows
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or 'rounded'
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- see ../after/plugin/lspconfig.lua for individual LSP configs
