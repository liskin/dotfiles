local fzf_lua = require'fzf-lua'

fzf_lua.setup {
	'fzf-native',
	fzf_colors = true,

	winopts = {
		preview = {
			horizontal = "right:60%,border-left",
			vertical = "down:45%,border-top",
		},

		width = 1,
		height = 0.80,
		row = 1,
	},

	defaults = {
		file_icons = false,
	},

	lsp = {
		symbols = {
			symbol_style = 3,
		},
	}
}

fzf_lua.register_ui_select()

vim.keymap.set('n', 'gX', fzf_lua.lsp_finder)
vim.keymap.set('n', 'gT', fzf_lua.lsp_document_symbols)
vim.keymap.set('n', 'gG', fzf_lua.lsp_live_workspace_symbols)

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local buf_local = { buffer = args.buf }
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if not client then
			return
		end

		if client.supports_method('textDocument/documentSymbol', { bufrn = args.buf }) then
			vim.keymap.set({'n'}, '<C-T>', fzf_lua.lsp_document_symbols, buf_local)
		end

		if client.supports_method('workspace/symbol') then
			vim.keymap.set({'n'}, '<C-G>', fzf_lua.lsp_live_workspace_symbols, buf_local)
		end
	end,
})
