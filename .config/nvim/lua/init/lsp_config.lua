local null_ls = require 'null-ls'

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
vim.lsp.config('lua_ls', {
	-- textDocument/diagnostics handler not registered without --preview=true
	cmd = { 'lua-language-server', '--preview=true' },

	settings = {
		Lua = {
			diagnostics = {
				-- do not automatically refresh workspace diagnostics
				workspaceEvent = "None",
			},
		},
	},

	on_init = function(client, init_result)
		-- allow pulling workspace diagnostics explicitly
		-- (lua_ls doesn't declare this capability itself for whatever reason)
		client.server_capabilities.diagnosticProvider.workspaceDiagnostics = true
	end,
})

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
vim.lsp.config('ruff', {
	init_options = {
		settings = {
			configurationPreference = "filesystemFirst",

			-- defaults
			lineLength = 120,
		},
	},
})
vim.lsp.enable('pylsp')

-- Tilt (Starlark)
vim.lsp.enable('tilt_ls')
vim.g.ft_autoformat_tiltfile = true

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

-- YAML
vim.lsp.enable('yamlls')

-- Zizmor (GitHub Actions YAML)
vim.lsp.enable('zizmor')

-- Justfile
vim.lsp.enable('just')

-- none-ls/null-ls
vim.g.lsp_null_enabled = {
	hadolint = { 'diagnostics' },
	buildifier = { 'diagnostics' },
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
