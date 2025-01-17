local cmp = require'cmp'
local cmp_nvim_ultisnips_source = require'cmp_nvim_ultisnips.source'
local snippet_noselectmode = require'init.lib.snippet_noselect'.snippet_noselectmode

-- avoid Select mode after snippet expansion
---@diagnostic disable-next-line: duplicate-set-field
function cmp_nvim_ultisnips_source:execute(completion_item, callback)
	vim.call("UltiSnips#ExpandSnippet")
	snippet_noselectmode()
	callback(completion_item)
end

local function has_words_before()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local function cmp_or(if_cmp, fallback1)
	return function(fallback2)
		if vim.fn.pumvisible() ~= 0 then
			fallback2()
		elseif cmp.visible() then
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

local function cmp_complete_ultisnips()
	cmp.complete {
		config = {
			sources = {
				{ name = 'ultisnips' },
			},
			snippet = {
				expand = function(args)
					vim.fn["UltiSnips#Anon"](args.body)
					snippet_noselectmode()
				end,
			},
		},
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
	window = {
		completion = {
			scrollbar = false,
			border = { '', '', '', '│' },
		},
		documentation = {
			border = { '', '', '', '│', '', '', '', ' ' },
		},
	},
	sources = cmp.config.sources(
		{
			{ name = 'nvim_lsp' },
			{ name = 'omni' },
			{ name = 'path' },
			{ name = 'conventionalcommits' },
		}, {
			{ name = 'buffer' },
			{ name = 'spell' },
		}
	),
	mapping = {
		['<Tab>'] = function(fallback)
			if vim.fn.pumvisible() ~= 0 then
				fallback()
			elseif vim.fn.maparg('<Plug>(tab_complete)', 'i') ~= '' then
				local keys = vim.api.nvim_replace_termcodes('<Plug>(tab_complete)', true, false, true)
				vim.api.nvim_feedkeys(keys, 'im', false)
			elseif cmp.visible() then
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
		['<CR>'] = cmp_or(cmp.confirm),
		['<C-N>'] = cmp_or(cmp_select_next, cmp_complete_buffer_all),
		['<C-P>'] = cmp_or(cmp_select_prev, cmp_complete_buffer_all),
		['<C-X><C-N>'] = cmp_or(cmp_select_next, cmp_complete_buffer_current),
		['<C-X><C-P>'] = cmp_or(cmp_select_prev, cmp_complete_buffer_current),
		['<C-X><C-O>'] = cmp_or(cmp_select_next, cmp_complete_omni),
		['<C-_>'] = cmp_or(cmp_select_next, cmp_complete_ultisnips),
		['<LeftMouse>'] = cmp_or(function() end),
	},
	---@diagnostic disable-next-line: missing-fields
	confirmation = {
		get_commit_characters = function(commit_characters)
			return {
				' ',
				'.', ',',
				':', ';',
				'(', ')',
				'[', ']',
				'{', '}',
				unpack(commit_characters)
			}
		end
	},
	snippet = {
		expand = function(args)
			vim.snippet.expand(args.body)
			snippet_noselectmode()
		end,
	},
}

cmp.event:on('menu_opened', function(data)
	-- start with an active entry
	-- work around https://github.com/hrsh7th/nvim-cmp/issues/835
	data.window.active = true
end)

-- smart Tab - snippet placeholders if any
vim.keymap.set({ 'n', 'v' }, '<Tab>', function()
	if vim.snippet.active({ direction = 1 }) then
		return '<Plug>(vim_snippet_next_noselectmode)'
	elseif vim.fn['UltiSnips#CanJumpForwards']() ~= 0 then
		return '<Plug>(ultisnip_next_noselectmode)'
	else
		return '<Tab>'
	end
end, { expr = true })
vim.keymap.set({ 'n', 'v' }, '<S-Tab>', function()
	if vim.snippet.active({ direction = -1 }) then
		return '<Plug>(vim_snippet_prev_noselectmode)'
	elseif vim.fn['UltiSnips#CanJumpBackwards']() ~= 0 then
		return '<Plug>(ultisnip_prev_noselectmode)'
	else
		return '<S-Tab>'
	end
end, { expr = true })
