require'lsp_lines'.setup()

---@class vim.diagnostic.Opts
---@field virtual_lines? boolean|OptsVirtualLines

local virtual_lines_config = {
	only_current_line = true,
}
vim.diagnostic.config { virtual_lines = virtual_lines_config }

vim.api.nvim_create_user_command("LspLinesToggle", function()
	if vim.diagnostic.config().virtual_lines then
		vim.diagnostic.config { virtual_lines = false }
	else
		vim.diagnostic.config { virtual_lines = virtual_lines_config }
	end
end, {
	desc = "Toggle lsp_lines",
})
