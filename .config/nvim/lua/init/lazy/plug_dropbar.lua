return {
	'dropbar.nvim',
	cmd = 'DropbarToggle',
	after = function()
		require 'init.plug_dropbar'
	end,
}
