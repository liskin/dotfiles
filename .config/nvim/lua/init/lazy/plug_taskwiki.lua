return {
	'taskwiki',
	event = {
		event = "BufReadPre",
		pattern = vim.env.HOME .. "/taskwiki/*",
	},
	before = function()
		require('lz.n').trigger_load('vimwiki')
	end
}
