local trouble = require 'trouble'

trouble.setup {
	warn_no_results = false,
	open_no_results = true,
	auto_preview = false,
	icons = {
		indent = {
			fold_open = "▾ ",
			fold_closed = "▸ ",
		},
		folder_closed = "",
		folder_open = "",
		kinds = {
			Array         = "[]",
			Boolean       = "bo",
			Class         = "Cl",
			Constant      = "==",
			Constructor   = "cs",
			Enum          = "__",
			EnumMember    = "_.",
			Event         = "ev",
			Field         = "..",
			File          = "//",
			Function      = "fn",
			Interface     = "In",
			Key           = "ke",
			Method        = "->",
			Module        = "Mo",
			Namespace     = "Na",
			Null          = "00",
			Number        = "12",
			Object        = "()",
			Operator      = "<>",
			Package       = "Pa",
			Property      = ".?",
			String        = '""',
			Struct        = "{}",
			TypeParameter = "Ty",
			Variable      = "= ",
		},
	},
	modes = {
		diagnostics = {
			groups = {
				{ "directory", format = "{directory} {count}" },
				{ "filename", format = "{basename} {count}" },
			},
		},
		lsp_base = {
			groups = {
				{ "filename", format = "{filename} {count}" },
			},
		},
		lsp_document_symbols = {
			groups = {
				{ "filename", format = "{filename} {count}" },
			},
			format = "{kind_icon} {symbol.name} {pos}",
		},
		qflist = {
			groups = {
				{ "filename", format = "{filename} {count}" },
			},
		},
		loclist = {
			groups = {
				{ "filename", format = "{filename} {count}" },
			},
		},
		symbols = {
			win = { position = "left" },
		}
	},
}
