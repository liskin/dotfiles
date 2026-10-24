return {
	'presenting.nvim',
	cmd = {
		'Present',
	},
	after = function()
		require 'init.plug_presenting'
	end,
}
