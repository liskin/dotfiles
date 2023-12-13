if vim.g.loaded_after_lspconfig then return end
vim.g.loaded_after_lspconfig = true

local cmp_nvim_lsp = require 'cmp_nvim_lsp'
local lsp_format = require 'lsp-format'
local lspconfig = require 'lspconfig'
local neodev = require 'neodev'
local null_ls = require 'null-ls'

local function is_nvim_path(path)
	local config_root = vim.fn.stdpath("config")
	local data_root = vim.fn.stdpath("data")
	return vim.startswith(path, config_root) or vim.startswith(path, data_root)
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
	end
end)

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
for tool, handlers in pairs(vim.g.lsp_null_enabled) do
	for _, handler in ipairs(handlers) do
		local source = vim.tbl_get(null_ls.builtins, handler, tool)
		if source then
			local settings = lsp_null_settings[handler .. '.' .. tool] or lsp_null_settings[tool]
			table.insert(lsp_null_sources, source.with(settings or {}))
		end
	end
end

null_ls.setup {
	sources = lsp_null_sources,
	on_attach = lsp_format.on_attach,
}
