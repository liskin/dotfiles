vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(args)
		vim.keymap.set({ 'n', 'v', 'i' }, '<C-F>', vim.lsp.buf.code_action, {
			buffer = args.buf,
			desc = "LSP code action",
		})

		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		if client.name == "copilot" and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlineCompletion, args.buf) then
			vim.lsp.inline_completion.enable(true, { bufnr = args.buf })

			vim.keymap.set('i', '<C-Space>', vim.lsp.inline_completion.get, {
				buffer = args.buf,
				desc = "LSP: accept inline completion",
			})
			vim.keymap.set('i', '<C-X>]', function() vim.lsp.inline_completion.select({ count = 1 }) end, {
				buffer = args.buf,
				desc = "LSP: next inline completion",
			})
			vim.keymap.set('i', '<C-X>[', function() vim.lsp.inline_completion.select({ count = -1 }) end, {
				buffer = args.buf,
				desc = "LSP: prev inline completion",
			})
		end
	end,
})

-- :LspCodeLensToggle
vim.api.nvim_create_user_command("LspCodeLensToggle", function()
	local codelens_enabled = not vim.lsp.codelens.is_enabled()
	vim.notify("LSP code lenses " .. (codelens_enabled and "enabled" or "disabled"))
	vim.lsp.codelens.enable(codelens_enabled)
end, {
	desc = "Toggle LSP code lenses",
})

-- :LspInlayToggle
vim.api.nvim_create_user_command("LspInlayToggle", function()
	local enable = not vim.lsp.inlay_hint.is_enabled()
	vim.lsp.inlay_hint.enable(enable)
	vim.notify("LSP inlay hints " .. (enable and "enabled" or "disabled"))
end, {
	desc = "Toggle LSP inlay hints",
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
	desc = "Toggle LSP reference highlights",
})

-- :LspCopilotToggle
vim.api.nvim_create_user_command("LspCopilotToggle", function()
	local enabled = not vim.lsp.is_enabled("copilot")
	vim.lsp.enable("copilot", enabled)
	vim.notify("LSP copilot " .. (enabled and "enabled" or "disabled"))
end, {
	desc = "Toggle copilot-language-server",
})

-- :LspWorkspaceDiagnostics
vim.api.nvim_create_user_command("LspWorkspaceDiagnostics", function()
	vim.lsp.buf.workspace_diagnostics()
	vim.notify("LSP - pulling workspace diagnostics")
end, {
	desc = "Pull LSP workspace diagnostics",
})

-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
vim.lsp.config('*', {
	flags = {
		debounce_text_changes = vim.g.lsp_debounce,
	},
})

-- disable vim.lsp.document_color because it only sets gui colors
vim.lsp.document_color.enable(false)

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
