local cmp = require'cmp'

local function has_words_before()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local function cmp_or(if_cmp, fallback1)
	return function(fallback2)
		if cmp.visible() then
			if_cmp()
		else
			(fallback1 or fallback2)()
		end
	end
end

local function cmp_select_next()
	cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
	if not cmp.get_selected_entry() then
		-- skip non-active state on wrap-around
		cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
	end
end

local function cmp_select_next_page()
	local menu_info = cmp.core.view:_get_entries_view():info()
	cmp.select_next_item({ behavior = cmp.SelectBehavior.Select, count = menu_info.inner_height })
	if not cmp.get_selected_entry() then
		-- skip non-active state on wrap-around
		cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
	end
end

local function cmp_select_prev()
	cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
	if not cmp.get_selected_entry() then
		-- skip non-active state on wrap-around
		cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
	end
end

local function cmp_select_prev_page()
	local menu_info = cmp.core.view:_get_entries_view():info()
	cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select, count = menu_info.inner_height })
	if not cmp.get_selected_entry() then
		-- skip non-active state on wrap-around
		cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
	end
end

local function cmp_complete_sources(sources)
	cmp.complete {
		config = {
			sources = sources,
		},
	}
end

local function cmp_complete_buffer_current()
	cmp_complete_sources {
		{ name = 'buffer' },
	}
end

local function cmp_complete_buffer_all()
	cmp_complete_sources {
		{
			name = 'buffer',
			option = {
				get_bufnrs = vim.api.nvim_list_bufs
			},
		},
	}
end

local function cmp_complete_omni()
	cmp_complete_sources {
		{ name = 'omni' },
	}
end

local function cmp_complete_snippet()
	cmp_complete_sources {
		{ name = 'ultisnips' },
	}
end

cmp.setup {
	completion = {
		autocomplete = false,
		completeopt = vim.o.completeopt,
	},
	experimental = {
		ghost_text = true,
	},
	snippet = {
		expand = function(args)
			vim.fn["UltiSnips#Anon"](args.body)
		end,
	},
	sources = cmp.config.sources(
		{
			{ name = 'nvim_lsp' },
			{ name = 'omni' },
			{ name = 'path' },
		}, {
			{ name = 'buffer' },
			{ name = 'spell' },
		}
	),
	mapping = {
		['<Tab>'] = function(fallback)
			if cmp.visible() then
				if #cmp.get_entries() == 1 then
					cmp.confirm()
				else
					cmp_select_next()
				end
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end,
		['<S-Tab>'] = cmp_or(cmp_select_prev),
		['<Down>'] = cmp_or(cmp_select_next),
		['<Up>'] = cmp_or(cmp_select_prev),
		['<PageDown>'] = cmp_or(cmp_select_next_page),
		['<PageUp>'] = cmp_or(cmp_select_prev_page),
		['<Right>'] = cmp_or(cmp.confirm),
		['<Left>'] = function(fallback)
			cmp.abort()
			fallback()
		end,
		['<C-N>'] = cmp_or(cmp_select_next, cmp_complete_buffer_all),
		['<C-P>'] = cmp_or(cmp_select_prev, cmp_complete_buffer_all),
		['<C-X><C-N>'] = cmp_or(cmp_select_next, cmp_complete_buffer_current),
		['<C-X><C-P>'] = cmp_or(cmp_select_prev, cmp_complete_buffer_current),
		['<C-X><C-O>'] = cmp_or(cmp_select_prev, cmp_complete_omni),
	},
	---@diagnostic disable-next-line: missing-fields
	confirmation = {
		get_commit_characters = function(commit_characters)
			return {
				' ', '\r',
				'.', ',',
				':', ';',
				'(', ')',
				'[', ']',
				'{', '}',
				unpack(commit_characters)
			}
		end
	},
}

cmp.event:on('menu_opened', function(data)
	-- start with an active entry
	-- work around https://github.com/hrsh7th/nvim-cmp/issues/835
	data.window.active = true
end)

vim.keymap.set('i', '<Plug>(cmp_snippet)', function()
	cmp_complete_snippet()
end)
