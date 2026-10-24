return {
	'codecompanion.nvim',
	cmd = {
		'CodeCompanion',
		'CodeCompanionChat',
	},
	keys = {
		{ '<Leader>CC', desc = "codecompanion.nvim actions" },
	},
	after = function()
		require 'init.plug_codecompanion'
	end,
}
