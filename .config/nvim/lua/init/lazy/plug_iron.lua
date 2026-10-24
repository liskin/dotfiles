return {
	'iron.nvim',
	cmd = {
		'IronRepl',
		'IronReplHere',
		'IronRestart',
		'IronSend',
		'IronFocus',
		'IronHide',
		'IronWatch',
		'IronAttach',
	},
	after = function()
		require 'init.plug_iron'
	end,
}
