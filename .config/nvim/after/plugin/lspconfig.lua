if vim.g.loaded_after_lspconfig then return end
vim.g.loaded_after_lspconfig = true

local lspconfig = require'lspconfig'
local null_ls = require'null-ls'
local lsp_format = require'lsp-format'

local function fullpaths(paths)
	local function full(path)
		return vim.fn.fnamemodify(vim.fn.resolve(path), ":p")
	end
	return vim.tbl_map(full, paths)
end

local function has_vimrc_vimdir(paths)
	local vimrc = vim.fn.expand("~/.vimrc")
	local vimdir = vim.fn.expand("~/.vim/")
	local nvimdir = vim.fn.expand("~/.config/nvim/")
	local function is_vim(path)
		return path == vimrc or vim.startswith(path, vimdir) or vim.startswith(path, nvimdir)
	end
	return not vim.tbl_isempty(vim.tbl_filter(is_vim, paths))
end

if has_vimrc_vimdir(fullpaths(vim.fn.argv())) then
	vim.g.lsp_autostart_lua_ls = true
	vim.g.lsp_settings_lua_ls = {
		Lua = {
			runtime = {
				version = 'LuaJIT'
			},
			-- Make the server aware of Neovim runtime files
			workspace = {
				checkThirdParty = false,
				library = {
					vim.fn.stdpath("config"),
					vim.env.VIMRUNTIME,
				},
				-- or pull in all of 'runtimepath'. NOTE: this is a lot slower
				-- library = vim.api.nvim_get_runtime_file("", true),
			}
		}
	}
end

lspconfig.util.on_setup = lspconfig.util.add_hook_after(lspconfig.util.on_setup, function(config)
	-- flake8_lint in pylsp needs root_dir, so add a fallback to the directory of the file
	if config.name == "pylsp" then
		local root_dir = config.root_dir
		config.root_dir = function(fname)
			return root_dir(fname) or vim.fs.dirname(fname)
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
