vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		vim.keymap.set({ 'n', 'v', 'i' }, '<C-F>', vim.lsp.buf.code_action, {
			buffer = ev.buf,
			desc = "LSP code action",
		})
	end,
})

-- :LspCodeLensToggle
local codelens_enabled = false
vim.api.nvim_create_autocmd('BufWritePost', {
	group = vim.api.nvim_create_augroup('LspCodeLensRefresh', {}),
	callback = function()
		if codelens_enabled then
			vim.lsp.codelens.refresh()
		end
	end
})
vim.api.nvim_create_user_command("LspCodeLensToggle", function()
	codelens_enabled = not codelens_enabled
	vim.notify("LSP code lenses " .. (codelens_enabled and "enabled" or "disabled"))
	if codelens_enabled then
		vim.lsp.codelens.refresh()
	else
		vim.lsp.codelens.clear(nil, nil)
	end
end, {
	desc = "Toggle LSP code lenses"
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

-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
vim.lsp.config('*', {
	flags = {
		debounce_text_changes = vim.g.lsp_debounce,
	},
})

-- force longer debounce for vim.lsp.semantic_tokens (not configurable otherwise)
-- to avoid wasting too much CPU in the LSP
local orig_vim_lsp_semantic_tokens_start = vim.lsp.semantic_tokens.start
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.semantic_tokens.start(bufnr, client_id, opts)
	opts = opts or {}
	opts.debounce = vim.g.lsp_debounce
	return orig_vim_lsp_semantic_tokens_start(bufnr, client_id, opts)
end

-- implement "should_attach" for vim.lsp.start
vim.lsp.start = (function(orig)
	return function(config, opts)
		opts = opts or {}
		local bufnr = vim._resolve_bufnr(opts.bufnr)

		if vim.b[bufnr].lsp_disabled or vim.g.lsp_disabled then
			return
		end

		return orig(config, opts)
	end
end)(vim.lsp.start)

-- see "lsp_config" and ~/.vim/projectrc for LSP config
-- see "plug_none_ls" for null-ls/none-ls
