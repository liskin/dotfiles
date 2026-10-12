-- avoid Select mode after snippet expansion

vim.keymap.set({ 'n', 'v', 's', 'i' }, '<Plug>(noselectmode)', function()
	if vim.fn.mode() == 's' then
		return '<C-G>'
	else
		return ''
	end
end, { expr = true })

vim.keymap.set({ 'n', 'v', 's', 'i' }, '<Plug>(exitvisualmode)', function()
	local mode = vim.fn.mode()
	if mode == 'v' or mode == 'V' then
		return '<Esc>'
	else
		return ''
	end
end, { expr = true })

local function snippet_noselectmode()
	local keys = vim.api.nvim_replace_termcodes('<Plug>(noselectmode)', true, false, true)
	vim.api.nvim_feedkeys(keys, '', false)
end

local function snippet_exitvisualmode()
	local keys = vim.api.nvim_replace_termcodes('<Plug>(exitvisualmode)', true, false, true)
	vim.api.nvim_feedkeys(keys, '', false)
end

-- hack to revert https://github.com/neovim/neovim/pull/41562 (session cancel on Esc);
-- needed after snippet expansion and every jump
local function snippet_keepsession()
	vim.api.nvim_clear_autocmds({ event = 'ModeChanged', group = 'nvim.snippet', buffer = 0 })
end

vim.keymap.set({ 'n', 'v' }, '<Plug>(vim_snippet_next_noselectmode)', function()
	snippet_exitvisualmode()
	vim.snippet.jump(1)
	snippet_keepsession()
	snippet_noselectmode()
end)
vim.keymap.set({ 'n', 'v' }, '<Plug>(vim_snippet_prev_noselectmode)', function()
	snippet_exitvisualmode()
	vim.snippet.jump(-1)
	snippet_keepsession()
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
	snippet_noselectmode = snippet_noselectmode,
	snippet_keepsession = snippet_keepsession,
}
