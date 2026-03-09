local fzf_lua = require'fzf-lua'
local lib_lsp = require'init.lib.lsp'

-- fzf-lua.actions.ex_run_cr that doesn't hide output from commands like
-- :version, :echo, etc.
local function ex_run_cr(selected)
	if not selected[1] then return end
	local cmd = selected[1]
	vim.cmd("stopinsert")
	local keys = string.format("<Cmd>%s<CR>", cmd)
	keys = vim.api.nvim_replace_termcodes(keys, true, false, true)
	vim.api.nvim_feedkeys(keys, 'nt', false)
	return cmd
end

fzf_lua.setup {
	'fzf-native',
	fzf_colors = true,

	winopts = {
		preview = {
			flip_columns = 120,
			horizontal = "right:67%,border-left",
			vertical = "down:45%,border-top",
		},

		width = 1,
		height = 0.80,
		row = 1,
	},

	defaults = {
		file_icons = false,
	},

	files = {
		hidden = true,
		follow = true,
		fd_opts = '--no-require-git ' .. fzf_lua.config.defaults.files.fd_opts,
	},

	grep = {
		hidden = true,
		follow = true,
		rg_opts = '--glob=!.git --no-require-git ' .. fzf_lua.config.defaults.grep.rg_opts,
	},

	lsp = {
		symbols = {
			symbol_style = 3,
			parent_postfix = ".",
		},
	},

	commands = {
		actions = {
			["ctrl-x"] = ex_run_cr,
		},
	},

	ui_select = true,
}

vim.keymap.set('n', 'gX', fzf_lua.lsp_finder, { desc = "fzf lsp_finder" })

vim.api.nvim_create_user_command("FzfLuaBTags", function()
	if lib_lsp.any_supports_method('textDocument/documentSymbol') then
		return fzf_lua.lsp_document_symbols()
	else
		return fzf_lua.btags()
	end
end, {
	desc = "Smart FzfLua btags - LSP, treesitter, buffer ctags fallback"
})

vim.api.nvim_create_user_command("FzfLuaTags", function()
	if lib_lsp.any_supports_method('workspace/symbol') then
		return fzf_lua.lsp_live_workspace_symbols()
	else
		return fzf_lua.tags()
	end
end, {
	desc = "Smart FzfLua tags - LSP, ctags fallback"
})
