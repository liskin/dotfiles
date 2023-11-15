if vim.g.loaded_after_lspconfig then return end
vim.g.loaded_after_lspconfig = true

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
	require'lspconfig'[lsp].setup {
		autostart = vim.g['lsp_autostart_' .. lsp] or false,
		settings = vim.g['lsp_settings_' .. lsp],
		cmd = vim.g['lsp_cmd_' .. lsp],
	}
end
