local lazydev = require'lazydev'
local lazydev_buf = require'lazydev.buf'
local lazydev_workspace = require'lazydev.workspace'

lazydev.setup {
	enabled = function(root_dir)
		return root_dir == vim.fn.stdpath("config")
	end,
	library = {
		{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
	},
}

local function root_dir(bufnr, on_dir)
	-- attach to existing workspace if possible
	local ws = lazydev.find_workspace(bufnr)
	if ws ~= nil then
		return on_dir(ws)
	end

	-- use ~/.config/nvim for everything nvim-related as that seems to work best
	local buf_name = vim.api.nvim_buf_get_name(bufnr)
	if
		vim.fs.relpath(vim.fn.stdpath("config"), buf_name)
		or vim.fs.relpath(vim.fn.stdpath("data"), buf_name)
		or vim.fs.relpath(vim.env.VIMRUNTIME, buf_name)
	then
		return on_dir(vim.fn.stdpath("config"))
	end

	-- fallback to default (luarc, git, …)
	return on_dir(nil)
end

vim.lsp.config('lua_ls', { root_dir = root_dir })
vim.schedule(function()
	vim.lsp.config('lua_ls', { root_dir = root_dir })
end)

lazydev_buf.on_file = (function(orig)
	return function(buf)
		-- add plugins being edited to the library
		local buf_name = vim.api.nvim_buf_get_name(buf)
		if vim.fs.relpath(vim.fn.stdpath("data"), buf_name) then
			local lua = buf_name:find("/lua/", 1, true)
			if lua then
				local path = buf_name:sub(1, lua - 1)
				lazydev_workspace.find({ buf = buf }):add(path)
			end
		end

		return orig(buf)
	end
end)(lazydev_buf.on_file)
