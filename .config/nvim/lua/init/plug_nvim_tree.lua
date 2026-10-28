local nvim_tree = require'nvim-tree'

nvim_tree.setup {
	view = {
		signcolumn = "auto",
	},

	renderer = {
		indent_markers = {
			enable = true,
		},
		icons = {
			show = {
				file = false,
				folder = false,
			},
			git_placement = "after",

			glyphs = {
				folder = {
					arrow_closed = "+", -- "▸",
					arrow_open = "-", -- "▾",
				},
				git = {
					unstaged = "m",
					staged = "M",
					unmerged = "C",
					renamed = "→",
					untracked = "?",
					deleted = "D",
					ignored = "i",
				},
			},
			symlink_arrow = " → ",
		},

		group_empty = true,
		add_trailing = true,
		hidden_display = "all",
		full_name = true,

		special_files = {
			"README.md",
			"Makefile",
			"Justfile",
			"pyproject.toml",
			"Cargo.toml",
		},
	},

	diagnostics = {
		enable = true,
		show_on_dirs = true,

		icons = {
			hint = "ⅰ",
			info = "i",
			warning = "!",
			error = "‼",
		},
	},

	actions = {
		use_system_clipboard = false,
	},

	sort = {
		sorter = "case_sensitive",
	},

	filters = {
		custom = {
			"^\\.git$",
		},
	},
}
