local presenting = require'presenting'
local lspconfig = require'lspconfig'
local follow_md_links = require'init.lib.follow-md-links'

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

local function quit()
	if restore_opts then
		restore_opts()
		restore_opts = nil
	end

	if present_in_argv() then
		vim.cmd.qa()
	else
		presenting.quit()
	end
end

local function is_image(filename)
	return vim.iter({
		".jpeg",
		".jpg",
		".png",
		".svg",
		".webp",
	}):any(function(ext) return vim.endswith(filename:lower(), ext) end)
end

local URI_SCHEME_PATTERN = '^([a-zA-Z]+[a-zA-Z0-9.+-]*):.*'
local function to_uri(cfile)
	if cfile:match(URI_SCHEME_PATTERN) then
		return cfile
	else
		return vim.uri_from_fname(cfile)
	end
end

local function view()
	local dest, type = follow_md_links.resolve_link_destination()
	if not dest or #dest == 0 then
		vim.notify("no link here")
		return
	end

	if is_image(dest) then
		vim.system({ 'feh', '--auto-zoom', '--image-bg', 'white', '--', dest })
	elseif type == "local" then
		-- FIXME: only for markdown and source files, open binaries using the browser or xdg-open (vim.ui.open())
		vim.cmd.tabe(dest)
	elseif type == "web" then
		vim.system({ 'google-chrome', '--app=' .. to_uri(dest) })
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
		['<Home>'] = presenting.first,
		['<End>'] = presenting.last,
		['<PageUp>'] = presenting.prev,
		['<PageDown>'] = presenting.next,
		['<CR>'] = view,
		['q'] = quit,
	},
	configure_slide_buffer = lspconfig.util.add_hook_after(presenting.config.configure_slide_buffer,
		function(_buf)
			configure_windows()
		end),
}

presenting.resize = lspconfig.util.add_hook_after(presenting.resize, function()
	configure_windows()
end)
