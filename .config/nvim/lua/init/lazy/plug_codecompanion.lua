return {
	'codecompanion.nvim',
	cmd = {
		'CodeCompanion',
		'CodeCompanionChat',
	},
	keys = {
		{ '<Leader>cc', desc = "codecompanion.nvim actions" },
	},
	after = function()
		require 'init.plug_codecompanion'
	end,
}
