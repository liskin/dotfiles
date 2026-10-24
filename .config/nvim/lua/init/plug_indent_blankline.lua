-- see also 'init.lazy.plug_indent_blankline'

require'ibl'.setup {
	indent = {
		char = '│',
	},
	scope = {
		show_start = false,
		show_end = false,
		exclude = {
			language = {
				'yaml',
			},
		},
	},
}
