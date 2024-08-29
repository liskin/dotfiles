vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		local buf_local = { buffer = ev.buf }

		vim.keymap.set({'n', 'v', 'i'}, '<C-H>', vim.lsp.buf.hover, buf_local)
		vim.keymap.set({'n', 'v', 'i'}, '<C-F>', vim.lsp.buf.code_action, buf_local)

		vim.keymap.set('i', '<C-K>', vim.lsp.buf.signature_help, buf_local)

		vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, buf_local)
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition, buf_local)
		vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition, buf_local)
		vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, buf_local)
		vim.keymap.set('n', 'gr', vim.lsp.buf.references, buf_local)
		vim.keymap.set('n', 'gR', vim.lsp.buf.rename, buf_local)
	end,
})

-- rounded borders of LSP floating windows
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or { '', '', '', '│' }
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- see "after_lsp" for individual LSP configs
