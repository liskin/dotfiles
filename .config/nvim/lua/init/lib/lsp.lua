local M = {}

function M.any_supports_method(method)
	local clients = vim.lsp.get_clients({ bufnr = 0 })
	return vim.iter(clients):any(function(client)
		return client:supports_method(method)
	end)
end

return M
