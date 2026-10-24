return {
	'trouble.nvim',
	cmd = {
		'Trouble',
	},
	after = function()
		require 'init.plug_trouble'
	end,
}
