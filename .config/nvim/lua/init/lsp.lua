vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		vim.keymap.set({ 'n', 'v', 'i' }, '<C-F>', vim.lsp.buf.code_action, {
			buffer = ev.buf,
			desc = "LSP code action",
		})

		-- neovim 0.11 mappings
		vim.keymap.set('n', 'grn', vim.lsp.buf.rename, {
			buffer = ev.buf,
			desc = "LSP rename",
		})
		vim.keymap.set('n', 'grr', vim.lsp.buf.references, {
			buffer = ev.buf,
			desc = "LSP references",
		})
		vim.keymap.set({ 'n', 'x' }, 'gra', vim.lsp.buf.code_action, {
			buffer = ev.buf,
			desc = "LSP code action",
		})
		vim.keymap.set('i', '<C-S>', vim.lsp.buf.signature_help, {
			buffer = ev.buf,
			desc = "LSP signature help",
		})
	end,
})

-- :LspInlayToggle
vim.api.nvim_create_user_command("LspInlayToggle", function()
	local enable = not vim.lsp.inlay_hint.is_enabled()
	vim.lsp.inlay_hint.enable(enable)
	vim.notify("LSP inlay hints " .. (enable and "enabled" or "disabled"))
end, {
	desc = "Toggle LSP inlay hints"
})

-- :LspReferenceHighlightsToggle
local refhi_enabled = false
local refhi_group = vim.api.nvim_create_augroup('LspReferenceHighlightsToggle', { clear = true })
vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
	group = refhi_group,
	callback = function()
		if refhi_enabled then
			local clients = vim.lsp.get_clients({ bufnr = 0 })
			local supported = vim.iter(clients):any(function(client)
				return client.supports_method('textDocument/documentHighlight')
			end)
			if supported then
				vim.lsp.buf.document_highlight()
			end
		end
	end,
})
vim.api.nvim_create_autocmd('CursorMoved', {
	group = refhi_group,
	callback = function() vim.lsp.buf.clear_references() end,
})
vim.api.nvim_create_user_command("LspReferenceHighlightsToggle", function()
	refhi_enabled = not refhi_enabled
	vim.notify("LSP reference highlights " .. (refhi_enabled and "enabled" or "disabled"))
end, {
	desc = "Toggle LSP reference highlights"
})

-- borders of LSP floating windows
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or { '', '', '', '│' }
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- see "after_lsp" for individual LSP configs
