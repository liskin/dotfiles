if vim.g.loaded_after_lspconfig then return end
vim.g.loaded_after_lspconfig = true

local function buffer_names()
	return vim.tbl_map(vim.api.nvim_buf_get_name, vim.api.nvim_list_bufs())
end

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

require'lspconfig'.lua_ls.setup{
	autostart = has_vimrc_vimdir(fullpaths(vim.fn.argv())),
	on_init = function (client)
		if has_vimrc_vimdir(fullpaths(buffer_names())) then
			client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
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
			})
			client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
		end

		return true
	end,
}
