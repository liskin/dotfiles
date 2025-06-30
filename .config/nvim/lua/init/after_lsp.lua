-- vim:set path^=./..:

local cmp_nvim_lsp = require 'cmp_nvim_lsp'
local lspconfig = require 'lspconfig'

lspconfig.util.on_setup = lspconfig.util.add_hook_after(lspconfig.util.on_setup, function(config)
	-- flake8_lint in pylsp needs root_dir, so add a fallback to the directory of the file
	if config.name == 'pylsp' then
		local orig_root_dir = config.root_dir
		config.root_dir = function(fname)
			return orig_root_dir(fname) or vim.fs.dirname(fname)
		end
	end

	config.flags = vim.tbl_deep_extend("keep", config.flags or {}, {
		-- don't waste CPU sending didChange too often, we won't see the diagnostics before save anyway
		debounce_text_changes = vim.g.lsp_debounce,
	})

	-- -- disable semantic tokens to avoid wasting CPU in the LSP
	-- config.on_attach = lspconfig.util.add_hook_after(config.on_attach, function(client, _bufnr)
	-- 	client.server_capabilities.semanticTokensProvider = nil
	-- end)
end)

local lsps = {
}

-- see '~/.vimrc' and 'init.lsp_settings' for the actual settings
for _, lsp in ipairs(lsps) do
	lspconfig[lsp].setup {
		autostart = vim.g['lsp_autostart_' .. lsp] or false,
		settings = vim.g['lsp_settings_' .. lsp],
		cmd = vim.g['lsp_cmd_' .. lsp],
		capabilities = cmp_nvim_lsp.default_capabilities(),
	}
end
