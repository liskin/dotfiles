return {
	'fzf-lua',
	event = {
		'DeferredUIEnter',
	},
	-- cmd = {
	-- 	'FzfLua',
	-- 	'FzfLuaBTags',
	-- 	'FzfLuaTags',
	-- },
	after = function()
		require 'init.plug_fzf'
	end,
}
