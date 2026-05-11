local zeal = require"zeal"

zeal.setup {
	picker = {
		type = "fzf-lua",
	},
	ft_map = {
		css = { "css" },
		html = { "html", "css" },
		javascript = { "javascript" },
		lua = { "lua_5.1", "neovim_contrib" },
		python = { "python_3" },
		rust = { "rust" },
		sh = { "bash" },
		vim = { "neovim_contrib" },
	},
}

vim.keymap.set('n', 'Za', function() zeal.search() end, { desc = "Search Zeal docs" })
vim.keymap.set('n', 'Zf', function() zeal.search_ft() end, { desc = "Search Zeal docs (current filetype)" })
vim.keymap.set('n', 'ZK', function()
	local query = vim.fn.expand('<cword>')
	zeal.search_ft(query)
end, { desc = "Search Zeal docs (current word, current filetype)" })
