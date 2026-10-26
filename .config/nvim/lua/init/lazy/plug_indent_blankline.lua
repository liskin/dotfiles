-- packadd not needed/used, the plugin is not in optbundle to allow its /after/ files to take effect;
-- only the setup is deferred

return {
	'indent-blankline.nvim',
	event = {
		'DeferredUIEnter',
	},
	after = function()
		require 'init.plug_indent_blankline'
	end,
}
