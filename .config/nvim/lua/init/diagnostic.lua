vim.diagnostic.config {
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = ">>",
			[vim.diagnostic.severity.WARN] = ">>",
			[vim.diagnostic.severity.INFO] = "――",
			[vim.diagnostic.severity.HINT] = "--",
		},
	},
	severity_sort = true,
	virtual_text = false,
}

local virtual_lines_off = {
	virtual_text = {
		-- current_line = true,
		virt_text_pos = 'eol_right_align',
	},
	virtual_lines = false,
}
local virtual_lines_on = {
	virtual_text = false,
	virtual_lines = {
		current_line = true,
	},
}
vim.diagnostic.config(virtual_lines_off)

vim.api.nvim_create_user_command("LspLinesToggle", function()
	if vim.diagnostic.config().virtual_lines then
		vim.diagnostic.config(virtual_lines_off)
		vim.cmd.windo("set signcolumn=no")
		vim.notify("LSP virtual_lines diagnostics disabled")
	else
		vim.diagnostic.config(virtual_lines_on)
		vim.cmd.windo("set signcolumn=auto")
		vim.notify("LSP virtual_lines diagnostics enabled")
	end
end, {
	desc = "Toggle lsp_lines",
})
