local null_ls = require 'null-ls'

-- XXX: nvim-0.11 only
if vim.fn.has('nvim-0.11') == 0 then
	return
end

local function buf_filesize(bufnr)
	local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(bufnr))
	if ok and stats then
		return stats.size
	else
		local content = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), '\n')
		return #content
	end
end

local function try_require(mod)
	local ok, source = pcall(require, mod)
	if ok then
		return source
	end
end

local lsp_null_sources = {}

for tool, handlers in pairs(vim.g.lsp_null_enabled) do
	for _, handler in ipairs(handlers) do
		local source =
			try_require('none-ls-' .. tool .. '.' .. handler) or
			try_require('none-ls.' .. handler .. '.' .. tool) or
			vim.tbl_get(null_ls.builtins, handler, tool)
		if source then
			local settings = vim.tbl_deep_extend(
				"keep",
				vim.g.lsp_null_settings[handler .. "." .. tool] or {},
				vim.g.lsp_null_settings[tool] or {}
			)
			table.insert(lsp_null_sources, source.with(settings))
		end
	end
end

null_ls.setup {
	sources = lsp_null_sources,
	-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
	-- (this needs to be the same as debounce_text_changes for LSPs, neovim uses the minimum)
	debounce = vim.g.lsp_debounce,
	should_attach = function(bufnr)
		-- disable for large files (FIXME: limit to proselint somehow?)
		local max_filesize = vim.g.lsp_maximum_file_size
		if buf_filesize(bufnr) > max_filesize then
			return false
		end

		if vim.b[bufnr].lsp_disabled or vim.g.lsp_disabled then
			return false
		end

		return true
	end,
}
