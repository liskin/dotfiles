return {
	'hop.nvim',
	cmd = {
		'Hop',
	},
	after = function()
		require 'init.plug_hop'
	end,
}
