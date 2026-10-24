return {
	'indent-blankline.nvim',
	event = {
		'DeferredUIEnter',
	},
	after = function()
		require 'init.plug_indent_blankline'
	end,
}
