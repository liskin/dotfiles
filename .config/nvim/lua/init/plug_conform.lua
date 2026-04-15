local conform = require'conform'

conform.setup {
	default_format_opts = {
		lsp_format = "fallback",
	},

	format_after_save = function(bufnr)
		if (vim.g.autoformat == nil or vim.g.autoformat) and vim.b[bufnr].autoformat then
			return {}
		end
	end,

	formatters_by_ft = {
		bzl = { 'buildifier' },
		sh = { 'shfmt' },
	},
}

vim.api.nvim_create_autocmd('LspAttach', {
	callback = function(args)
		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		if client:supports_method('textDocument/formatting') then
			if client.name == "null-ls" or vim.g['lsp_autoformat_' .. client.name] then
				vim.b[args.buf].autoformat = true
			end
		end
	end,
})

vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

vim.api.nvim_create_user_command("Conform", function()
	conform.format()
end, {})
