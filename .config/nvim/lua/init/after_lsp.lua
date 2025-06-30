-- vim:set path^=./..:

local cmp_nvim_lsp = require 'cmp_nvim_lsp'
local lspconfig = require 'lspconfig'
local null_ls = require 'null-ls'

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

lspconfig.util.on_setup = lspconfig.util.add_hook_after(lspconfig.util.on_setup, function(config)
	-- flake8_lint in pylsp needs root_dir, so add a fallback to the directory of the file
	if config.name == 'pylsp' then
		local orig_root_dir = config.root_dir
		config.root_dir = function(fname)
			return orig_root_dir(fname) or vim.fs.dirname(fname)
		end
	end

	config.flags = vim.tbl_deep_extend("keep", config.flags or {}, {
		-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
		debounce_text_changes = vim.g.lsp_debounce,
	})

	-- -- disable semantic tokens to avoid wasting CPU in the LSP
	-- config.on_attach = lspconfig.util.add_hook_after(config.on_attach, function(client, _bufnr)
	-- 	client.server_capabilities.semanticTokensProvider = nil
	-- end)
end)

local lsps = {
	'clangd',
	'elixirls',
	'gopls',
	'hls',
	'pylsp',
	'rust_analyzer',
	'taplo',
	'ts_ls',
}

-- see '~/.vimrc' and 'init.lsp_settings' for the actual settings
for _, lsp in ipairs(lsps) do
	lspconfig[lsp].setup {
		autostart = vim.g['lsp_autostart_' .. lsp] or false,
		settings = vim.g['lsp_settings_' .. lsp],
		cmd = vim.g['lsp_cmd_' .. lsp],
		capabilities = cmp_nvim_lsp.default_capabilities(),
	}
end

local lsp_null_sources = {}
local lsp_null_settings = vim.g.lsp_null_settings or {}

-- don't waste time proselinting before saving, we won't see the diagnostics before save anyway
lsp_null_settings['diagnostics.proselint'] = vim.tbl_deep_extend("keep", lsp_null_settings['diagnostics.proselint'] or {}, {
	method = {null_ls.methods.DIAGNOSTICS_ON_OPEN, null_ls.methods.DIAGNOSTICS_ON_SAVE}
})

for tool, handlers in pairs(vim.g.lsp_null_enabled) do
	for _, handler in ipairs(handlers) do
		local source =
			try_require('none-ls-' .. tool .. '.' .. handler) or
			try_require('none-ls.' .. handler .. '.' .. tool) or
			vim.tbl_get(null_ls.builtins, handler, tool)
		if source then
			local settings = vim.tbl_deep_extend("keep", lsp_null_settings[handler .. '.' .. tool] or {}, lsp_null_settings[tool] or {})
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
