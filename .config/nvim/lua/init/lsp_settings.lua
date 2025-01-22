local git_root = vim.fs.root(vim.uv.cwd() or error(), ".git")

local function has(filename)
	return git_root and vim.fn.filereadable(vim.fs.joinpath(git_root, filename))
end

if has('.taplo.toml') or has('taplo.toml') then
	vim.g.lsp_autoformat_taplo = true
end
