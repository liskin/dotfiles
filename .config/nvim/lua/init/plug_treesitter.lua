local ts = require'nvim-treesitter'

local ensure_installed = {
	"bash",
	"c",
	"c_sharp",
	"comment",
	"cpp",
	"css",
	"diff",
	"dockerfile",
	"fsharp",
	"git_config",
	"go",
	"graphql",
	"html",
	"javascript",
	"jsdoc",
	"json",
	"json5",
	"just",
	"lua",
	"markdown",
	"markdown_inline",
	"mermaid",
	"python",
	"query",
	"regex",
	"rust",
	"scss",
	"sql",
	"starlark",
	"vim",
	"vimdoc",
	"yaml",
}
local disabled = {
	"bash", -- too buggy (but stays enabled in injections)
}
local disabled_indent = {
}
local disabled_fold = {
}

vim.treesitter.language.register('starlark', { 'bzl.tiltfile' })

vim.api.nvim_create_user_command("TSUpdateSync", function()
	ts.install(ensure_installed):wait(300000)
	ts.update():wait(300000)
end, {})

vim.api.nvim_create_autocmd("FileType", {
	callback = function(ev)
		local bufnr = ev.buf
		local filetype = vim.api.nvim_get_option_value("filetype", { buf = bufnr })
		if not filetype or filetype == '' then
			return
		end

		-- skip if treesitter disabled for buffer (see FileTypeEarly in ~/.vimrc)
		if vim.b[bufnr].treesitter_disabled or vim.g.treesitter_disabled then
			return
		end

		-- skip if a parser for this language is disabled
		local parser_name = vim.treesitter.language.get_lang(filetype)
		if not parser_name or vim.list_contains(disabled, parser_name) then
			return
		end

		-- skip if a parser for this language isn't available
		local parser_ok, parser = pcall(vim.treesitter.get_parser, bufnr, parser_name)
		if not parser_ok or not parser then
			return
		end

		vim.treesitter.start(bufnr, parser_name)

		-- optionally re-enable regex-based syntax (see FileTypeEarly in ~/.vimrc)
		if vim.b[bufnr].treesitter_plus_regex then
			vim.bo[bufnr].syntax = 'ON'
		end

		-- set indentexpr if indents are enabled and available
		if
			not vim.list_contains(disabled_indent, parser_name)
			and not vim.tbl_isempty(vim.treesitter.query.get_files(parser_name, "indents"))
		then
			vim.bo[bufnr].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
		end

		-- set foldexpr if folds are enabled and available
		if
			not vim.list_contains(disabled_fold, parser_name)
			and not vim.tbl_isempty(vim.treesitter.query.get_files(parser_name, "folds"))
		then
			vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
			vim.wo.foldmethod = "expr"

			-- refresh folds
			local winnr = vim.api.nvim_get_current_win()
			vim.defer_fn(function()
				pcall(vim.api.nvim_win_call, winnr, function()
					vim.cmd("normal! zx")
				end)
			end, 500)
		end
	end,
})
