local lspconfig_util = require 'lspconfig.util'

local git_root = lspconfig_util.find_git_ancestor(vim.uv.cwd())

local function has(filename)
	return git_root and vim.fn.filereadable(vim.fs.joinpath(git_root, filename))
end

if has('.taplo.toml') or has('taplo.toml') then
	vim.g.lsp_autoformat_taplo = true
end
