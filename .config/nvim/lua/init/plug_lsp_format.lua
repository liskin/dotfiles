local lsp_format = require 'lsp-format'

lsp_format.setup {}

vim.api.nvim_create_user_command('LspFormat', function()
	vim.lsp.buf.format()
end, {})


vim.api.nvim_create_autocmd('LspAttach', {
	callback = function(args)
		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		if client.name == "null-ls" or vim.g['lsp_autoformat_' .. client.name] then
			lsp_format.on_attach(client, args.buf)
		end
	end,
})
