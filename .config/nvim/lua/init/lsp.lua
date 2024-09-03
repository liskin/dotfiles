vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		local buf_local = { buffer = ev.buf }

		vim.keymap.set({'n', 'v', 'i'}, '<C-F>', vim.lsp.buf.code_action, buf_local)

		-- neovim 0.11 mappings
		vim.keymap.set('n', 'grn', vim.lsp.buf.rename, buf_local)
		vim.keymap.set('n', 'grr', vim.lsp.buf.references, buf_local)
		vim.keymap.set({'n', 'x'}, 'gra', vim.lsp.buf.code_action, buf_local)
		vim.keymap.set('i', '<C-S>', vim.lsp.buf.signature_help, buf_local)
	end,
})

vim.keymap.set('n', '<Plug>(lsp_inlay_toggle)', function()
	vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end)

-- borders of LSP floating windows
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or { '', '', '', '│' }
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- see "after_lsp" for individual LSP configs
