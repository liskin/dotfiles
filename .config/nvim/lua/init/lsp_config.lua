local null_ls = require 'null-ls'

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

-- global
vim.g.lsp_debounce = 5000 -- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
vim.g.lsp_maximum_file_size = 131072 -- none-ls/null-ls only for now

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

-- Python
vim.lsp.config('pylsp', {
	settings = {
		pylsp = {
			configurationSources = { 'flake8' },
			plugins = {
				-- disable mypy by default to prevent .mypy_cache appearing all over the filesystem
				pylsp_mypy = { enabled = false },

				-- use flake8 (covers functionality of pyflakes, pycodestyle, mccabe)
				flake8 = { enabled = true },
				pyflakes = { enabled = false },
				pycodestyle = { enabled = false },
				mccabe = { enabled = false },

				-- disable black (formatting) and ruff (formatting + linting) by default, only enable isort
				isort = { enabled = true },
				black = { enabled = false },
				ruff = { enabled = false },
			},
		},
	},
})
vim.lsp.enable('pylsp')

-- Tilt (Starlark)
vim.lsp.enable('tilt_ls')
vim.lsp.config('tilt_ls', { filetypes = { 'bzl.tiltfile' } })

-- TOML
vim.lsp.enable('taplo')
local git_root = vim.fs.root(vim.uv.cwd() or error(), '.git')
if has_file(git_root, '.taplo.toml') or has_file(git_root, 'taplo.toml') then
	vim.g.lsp_autoformat_taplo = true
end

-- Elixir
vim.lsp.config('elixirls', {
	cmd = {vim.env.HOME .. '/src-elixir/.build/elixir-ls/language_server.sh'},
	settings = {
		elixirLS = {
			dialyzerEnabled = false,
		},
	},
})
vim.g.lsp_autoformat_elixirls = true

-- none-ls/null-ls
vim.g.lsp_null_enabled = {
	hadolint = { 'diagnostics' },
	buildifier = { 'diagnostics', 'formatting' },
	proselint = { 'code_actions', 'diagnostics' },
	shellcheck = { 'code_actions', 'diagnostics' },
}
vim.g.lsp_null_settings = {
	proselint = {
		filetypes = {
			'gitcommit',
			'mail',
			'markdown',
			'rst',
			'text',
		},
	},
	['diagnostics.proselint'] = {
		-- don't waste time proselinting before saving, we won't see the diagnostics before save anyway
		method = { null_ls.methods.DIAGNOSTICS_ON_OPEN, null_ls.methods.DIAGNOSTICS_ON_SAVE },
	},
}
