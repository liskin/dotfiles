-- avoid Select mode after snippet expansion

vim.keymap.set({ 'n', 'v', 's', 'i' }, '<Plug>(noselectmode)', function()
	if vim.fn.mode() == 's' then
		return '<C-G>'
	else
		return ''
	end
end, { expr = true })

local function snippet_noselectmode()
	local keys = vim.api.nvim_replace_termcodes('<Plug>(noselectmode)', true, false, true)
	vim.api.nvim_feedkeys(keys, '', false)
end

vim.keymap.set({ 'n', 'v' }, '<Plug>(vim_snippet_next_noselectmode)', function()
	vim.snippet.jump(1)
	snippet_noselectmode()
end)
vim.keymap.set({ 'n', 'v' }, '<Plug>(vim_snippet_prev_noselectmode)', function()
	vim.snippet.jump(-1)
	snippet_noselectmode()
end)
vim.keymap.set({ 'n', 'v' }, '<Plug>(ultisnip_next_noselectmode)', function()
	vim.fn['UltiSnips#JumpForwards']()
	snippet_noselectmode()
end)
vim.keymap.set({ 'n', 'v' }, '<Plug>(ultisnip_prev_noselectmode)', function()
	vim.fn['UltiSnips#JumpBackwards']()
	snippet_noselectmode()
end)

return {
	snippet_noselectmode = snippet_noselectmode
}
