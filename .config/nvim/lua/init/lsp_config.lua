-- XXX: nvim-0.11 only
if vim.fn.has('nvim-0.11') == 0 then
	return
end

local function has_file(path, filename)
	return path and vim.fn.filereadable(vim.fs.joinpath(path, filename)) == 1
end

local function has_dir(path, dirname)
	return path and vim.fn.isdirectory(vim.fs.joinpath(path, dirname)) == 1
end

-- Lua
vim.lsp.enable('lua_ls')

-- Rust
vim.lsp.config('rust_analyzer', {
	settings = {
		['rust-analyzer'] = {
			cargo = {
				features = 'all',
				allTargets = true,
			},
			check = {
				command = 'clippy',
				extraArgs = { '--no-deps' },
			},
		},
	},
})
local cargo_root = vim.fs.root(vim.uv.cwd() or error(), 'Cargo.toml')
if has_dir(cargo_root, 'target/debug') or has_dir(cargo_root, 'target/release') then
	vim.lsp.enable('rust_analyzer')
	vim.g.lsp_autoformat_rust_analyzer = true
end

-- Tilt (Starlark)
vim.lsp.enable('tilt_ls')
vim.lsp.config('tilt_ls', { filetypes = { 'bzl.tiltfile' } })

-- TOML
vim.lsp.enable('taplo')
local git_root = vim.fs.root(vim.uv.cwd() or error(), '.git')
if has_file(git_root, '.taplo.toml') or has_file(git_root, 'taplo.toml') then
	vim.g.lsp_autoformat_taplo = true
end
