local cmp_nvim_lsp = require 'cmp_nvim_lsp'
local lsp_format = require 'lsp-format'
local lspconfig = require 'lspconfig'
local lspconfig_manager = require 'lspconfig.manager'
local neodev = require 'neodev'
local neodev_util = require 'neodev.util'
local null_ls = require 'null-ls'

local function is_nvim_path(path)
	local config_root = vim.fn.stdpath("config")
	local data_root = vim.fn.stdpath("data")
	return vim.startswith(path, config_root) or vim.startswith(path, data_root)
end

local function buf_filesize(bufnr)
	local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
	if ok and stats then
		return stats.size
	else
		local content = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), '\n')
		return #content
	end
end

neodev.setup {
	override = function(root_dir, options)
		-- don't enable neodev for roots having a lua subdirectory that aren't in neovim dirs
		if not is_nvim_path(root_dir) then
			options.enabled = false
		end
	end,
}

lspconfig.util.on_setup = lspconfig.util.add_hook_after(lspconfig.util.on_setup, function(config)
	-- flake8_lint in pylsp needs root_dir, so add a fallback to the directory of the file
	if config.name == 'pylsp' then
		local root_dir = config.root_dir
		config.root_dir = function(fname)
			return root_dir(fname) or vim.fs.dirname(fname)
		end
	end

	-- nvim-lspconfig doesn't handle dot-separated filetypes (https://github.com/neovim/nvim-lspconfig/issues/1220)
	if config.name == 'tilt_ls' then
		config.filetypes = {'*.tiltfile', unpack(config.filetypes)}
	end

	if config.name == 'lua_ls' then
		-- workaround for nvim's incorrect handling of scopes in the workspace/configuration handler
		-- https://github.com/folke/neodev.nvim/issues/41
		-- https://github.com/LuaLS/lua-language-server/issues/1089
		-- https://github.com/LuaLS/lua-language-server/issues/1596
		local orig_handler = vim.lsp.handlers['workspace/configuration']
		---@diagnostic disable-next-line: duplicate-set-field
		vim.lsp.handlers['workspace/configuration'] = function(...)
			local _, result, ctx = ...
			local client_id = ctx.client_id
			local client = vim.lsp.get_client_by_id(client_id)
			if client and client.workspace_folders and #client.workspace_folders then
				if result.items and #result.items > 0 then
					if not result.items[1].scopeUri then
						return vim.tbl_map(function(_) return nil end, result.items)
					end
				end
			end

			return orig_handler(...)
		end

		-- resolve symlinks before looking for root_dir
		local orig_root_dir = config.root_dir
		config.root_dir = function(fname)
			return orig_root_dir(vim.loop.fs_realpath(fname) or fname)
		end

		-- resolve symlinks before looking for lua_root in neodev
		-- https://github.com/folke/neodev.nvim/commit/a34a9e7e775f1513466940c31285292b7b8375de#r134948856
		local orig_neodev_util_find_root = neodev_util.find_root
		---@diagnostic disable-next-line: duplicate-set-field
		function neodev_util.find_root(path)
			path = path or vim.api.nvim_buf_get_name(0)
			return orig_neodev_util_find_root(vim.loop.fs_realpath(path) or path)
		end
	end

	config.flags = vim.tbl_deep_extend("keep", config.flags or {}, {
		-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
		debounce_text_changes = 5000,
	})

	-- -- disable semantic tokens to avoid wasting CPU in the LSP
	-- config.on_attach = lspconfig.util.add_hook_after(config.on_attach, function(client, _bufnr)
	-- 	client.server_capabilities.semanticTokensProvider = nil
	-- end)
end)

-- implement "should_attach" for nvim-lspconfig
local orig_lspconfig_manager_add = lspconfig_manager.add
---@diagnostic disable-next-line: duplicate-set-field
function lspconfig_manager.add(self, root_dir, single_file, bufnr)
	if not vim.b[bufnr].lsp_disabled and not vim.g.lsp_disabled then
		return orig_lspconfig_manager_add(self, root_dir, single_file, bufnr)
	end
end

-- force longer debounce for vim.lsp.semantic_tokens (not configurable otherwise)
-- to avoid wasting too much CPU in the LSP
local orig_vim_lsp_semantic_tokens_start = vim.lsp.semantic_tokens.start
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.semantic_tokens.start(bufnr, client_id, opts)
	opts = opts or {}
	opts.debounce = 5000
	return orig_vim_lsp_semantic_tokens_start(bufnr, client_id, opts)
end

local lsps = {
	'clangd',
	'elixirls',
	'hls',
	'lua_ls',
	'pylsp',
	'rust_analyzer',
	'tilt_ls',
	'tsserver',
}

for _, lsp in ipairs(lsps) do
	lspconfig[lsp].setup {
		autostart = vim.g['lsp_autostart_' .. lsp] or false,
		settings = vim.g['lsp_settings_' .. lsp],
		cmd = vim.g['lsp_cmd_' .. lsp],
		on_attach = function(client, bufnr)
			if vim.g['lsp_autoformat_' .. lsp] then
				require'lsp-format'.on_attach(client, bufnr)
			end
		end,
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
		local source = vim.tbl_get(null_ls.builtins, handler, tool)
		if source then
			local settings = vim.tbl_deep_extend("keep", lsp_null_settings[handler .. '.' .. tool] or {}, lsp_null_settings[tool] or {})
			table.insert(lsp_null_sources, source.with(settings))
		end
	end
end

null_ls.setup {
	sources = lsp_null_sources,
	on_attach = lsp_format.on_attach,
	-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
	-- (this needs to be the same as debounce_text_changes for LSPs, neovim uses the minimum)
	debounce = 5000,
	should_attach = function(bufnr)
		-- disable for large files (FIXME: limit to proselint somehow?)
		local max_filesize = vim.g.ale_maximum_file_size
		if buf_filesize(bufnr) > max_filesize then
			return false
		end

		if vim.b[bufnr].lsp_disabled or vim.g.lsp_disabled then
			return false
		end

		return true
	end,
}
