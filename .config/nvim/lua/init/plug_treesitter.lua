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
	"git_config",
	"go",
	"graphql",
	"html",
	"javascript",
	"jsdoc",
	"json",
	"json5",
	"jsonc",
	"lua",
	"markdown",
	"markdown_inline",
	"mermaid",
	"python",
	"query",
	"regex",
	"rust",
	"sql",
	"vim",
	"vimdoc",
	"yaml",
}
local disabled = {
	-- "bash",
}

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

		local parser_name = vim.treesitter.language.get_lang(filetype)
		local parser_installed = pcall(vim.treesitter.get_parser, bufnr, parser_name)
		if parser_installed and not vim.list_contains(disabled, parser_name) then
			vim.treesitter.start(bufnr, parser_name)
		end
	end,
})
