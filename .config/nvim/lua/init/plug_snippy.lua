local snippy = require'snippy'
local snippet_noselect = require'init.lib.snippet_noselect'

snippy.setup {
	session_mappings = {
		n = {
			['<Esc>'] = "finish",
		},
	},
	-- virtual_markers = {
	-- 	enabled = true,
	-- 	default = ' $%n ',
	-- 	empty = ' $%n',
	-- },
}

-- snippy only maps these for i and s
vim.keymap.set({ 'n', 'v' }, '<Plug>(snippy-next)', function()
	snippet_noselect.snippet_exitvisualmode()
	snippy.next()
end)
vim.keymap.set({ 'n', 'v' }, '<Plug>(snippy-previous)', function()
	snippet_noselect.snippet_exitvisualmode()
	snippy.previous()
end)

vim.api.nvim_create_autocmd("User", {
	pattern = "SnippyJumped",
	callback = function(_)
		snippet_noselect.snippet_noselectmode()
	end,
})
