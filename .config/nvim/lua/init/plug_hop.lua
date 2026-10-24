-- see also 'init.lazy.plug_hop'

require'hop'.setup {
	extensions = {
		'hop-yank',
		'hop-treesitter',
		'init.lib.hop_treesitter_objects'
	},
}

vim.api.nvim_create_user_command("Hop", function(opts)
	vim.cmd("Hop" .. opts.args)
end, {
	nargs = "+",
})
