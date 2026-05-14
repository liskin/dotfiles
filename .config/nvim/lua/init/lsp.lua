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
if vim.fn.has('nvim-0.12') == 1 then
	vim.api.nvim_create_user_command("LspCodeLensToggle", function()
		local codelens_enabled = not vim.lsp.codelens.is_enabled()
		vim.notify("LSP code lenses " .. (codelens_enabled and "enabled" or "disabled"))
		vim.lsp.codelens.enable(codelens_enabled)
	end, {
		desc = "Toggle LSP code lenses"
	})
end

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
				return client:supports_method('textDocument/documentHighlight')
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

-- :LspWorkspaceDiagnostics
vim.api.nvim_create_user_command("LspWorkspaceDiagnostics", function()
	vim.lsp.buf.workspace_diagnostics()
	vim.notify("LSP - pulling workspace diagnostics")
end, {
	desc = "Pull LSP workspace diagnostics"
})

-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
vim.lsp.config('*', {
	flags = {
		debounce_text_changes = vim.g.lsp_debounce,
	},
})

-- force longer debounce for vim.lsp.semantic_tokens (not configurable otherwise)
-- to avoid wasting too much CPU in the LSP
if vim.fn.has('nvim-0.12') == 1 then
	vim.lsp.semantic_tokens.is_enabled()
	vim.lsp._capability.all.semantic_tokens.new = (function(orig)
		return function(...)
			local ret = orig(...)
			ret.debounce = vim.g.lsp_debounce
			return ret
		end
	end)(vim.lsp._capability.all.semantic_tokens.new)
else
	local orig_vim_lsp_semantic_tokens_start = vim.lsp.semantic_tokens.start
	---@diagnostic disable-next-line: duplicate-set-field
	function vim.lsp.semantic_tokens.start(bufnr, client_id, opts)
		opts = opts or {}
		opts.debounce = vim.g.lsp_debounce
		return orig_vim_lsp_semantic_tokens_start(bufnr, client_id, opts)
	end
end

-- disable vim.lsp.document_color because it only sets gui colors and has no
-- debounce at all, forcing the send of didChange to LSPs on every keystroke
if vim.fn.has('nvim-0.12') == 1 then
	vim.lsp.document_color.enable(false)
end

-- force sending didChange on save to servers that don't ask for didSave
vim.api.nvim_create_autocmd('BufWritePost', {
	group = vim.api.nvim_create_augroup('DidChangeFlush', {}),
	callback = function(ev)
		for _, client in pairs(vim.lsp.get_clients({ bufnr = ev.buf })) do
			require("vim.lsp._changetracking").flush(client, ev.buf)
		end
	end,
})

-- implement "should_attach" for vim.lsp.start
vim.lsp.start = (function(orig)
	return function(config, opts)
		opts = opts or {}
		local bufnr = vim._resolve_bufnr(opts.bufnr)

		-- to set this early enough, see FileTypeEarly in ~/.vimrc
		if vim.b[bufnr].lsp_disabled or vim.g.lsp_disabled then
			return
		end

		return orig(config, opts)
	end
end)(vim.lsp.start)

-- see "lsp_config" and ~/.vim/projectrc for LSP config
-- see "plug_none_ls" for null-ls/none-ls
