local presenting = require'presenting'
local lspconfig = require'lspconfig'

local function present_in_argv()
	return vim.tbl_contains(vim.v.argv, '-c') and vim.tbl_contains(vim.v.argv, 'Present')
end

if present_in_argv() then
	vim.g.lsp_disabled = 1
end

local restore_opts

vim.api.nvim_create_user_command('Present', function()
	do
		local guicursor = vim.o.guicursor
		local cmdheight = vim.o.cmdheight
		restore_opts = function()
			vim.o.guicursor = guicursor
			vim.o.cmdheight = cmdheight
		end
		vim.o.guicursor = 'a:hor1'
		vim.o.cmdheight = 0
	end

	presenting.config.options.footer_text = vim.fn.expand('%:t')
	vim.cmd.Presenting()
end, {})

local function configure_windows()
	local st = presenting._state
	if not st then
		return
	end

	for _, w in pairs({st.background_win, st.footer_win, st.slide_win}) do
		if w then
			vim.wo[w].winhighlight = 'NormalFloat:Normal'
		end
	end

	local w = st.slide_win
	if w then
		vim.wo[w].wrap = false
		vim.wo[w].conceallevel = 2

		local c = vim.api.nvim_win_get_config(w)
		vim.api.nvim_win_set_config(w, {
			relative = 'editor',
			row = 2,
			col = c.col,
		})
	end
end

presenting.setup {
	options = {
		width = 80,
	},
	keymaps = {
		['f'] = {},
		['l'] = {},
		['n'] = {},
		['p'] = {},
		['<Home>'] = function() presenting.first() end,
		['<End>'] = function() presenting.last() end,
		['<PageUp>'] = function() presenting.prev() end,
		['<PageDown>'] = function() presenting.next() end,
		['q'] = function()
			if restore_opts then
				restore_opts()
				restore_opts = nil
			end

			if present_in_argv() then
				vim.cmd.qa()
			else
				presenting.quit()
			end
		end,
	},
	configure_slide_buffer = lspconfig.util.add_hook_after(presenting.config.configure_slide_buffer,
		function(_buf)
			configure_windows()
		end),
}

presenting.resize = lspconfig.util.add_hook_after(presenting.resize, function()
	configure_windows()
end)
