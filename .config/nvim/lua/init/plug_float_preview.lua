vim.g['float_preview#docked'] = 0
vim.api.nvim_create_autocmd('User', {
	pattern = 'FloatPreviewWinOpen',
	callback = function()
		vim.api.nvim_win_set_config(vim.g['float_preview#win'], { border = { '', '', '', '│' } })
	end,
})
