" Remove all existing highlighting and set the defaults.
hi clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
	syntax reset
endif

let colors_name = "liskin"

let s:urxvt =
	\[ "#000000"
	\, "#cd0000"
	\, "#00cd00"
	\, "#cdcd00"
	\, "#0000cd"
	\, "#cd00cd"
	\, "#00cdcd"
	\, "#e5e5e5"
	\, "#4d4d4d"
	\, "#ff0000"
	\, "#00ff00"
	\, "#ffff00"
	\, "#0000ff"
	\, "#ff00ff"
	\, "#00ffff"
	\, "#ffffff"
	\, "#000000"
	\, "#00005f"
	\, "#000087"
	\, "#0000af"
	\, "#0000d7"
	\, "#0000ff"
	\, "#005f00"
	\, "#005f5f"
	\, "#005f87"
	\, "#005faf"
	\, "#005fd7"
	\, "#005fff"
	\, "#008700"
	\, "#00875f"
	\, "#008787"
	\, "#0087af"
	\, "#0087d7"
	\, "#0087ff"
	\, "#00af00"
	\, "#00af5f"
	\, "#00af87"
	\, "#00afaf"
	\, "#00afd7"
	\, "#00afff"
	\, "#00d700"
	\, "#00d75f"
	\, "#00d787"
	\, "#00d7af"
	\, "#00d7d7"
	\, "#00d7ff"
	\, "#00ff00"
	\, "#00ff5f"
	\, "#00ff87"
	\, "#00ffaf"
	\, "#00ffd7"
	\, "#00ffff"
	\, "#5f0000"
	\, "#5f005f"
	\, "#5f0087"
	\, "#5f00af"
	\, "#5f00d7"
	\, "#5f00ff"
	\, "#5f5f00"
	\, "#5f5f5f"
	\, "#5f5f87"
	\, "#5f5faf"
	\, "#5f5fd7"
	\, "#5f5fff"
	\, "#5f8700"
	\, "#5f875f"
	\, "#5f8787"
	\, "#5f87af"
	\, "#5f87d7"
	\, "#5f87ff"
	\, "#5faf00"
	\, "#5faf5f"
	\, "#5faf87"
	\, "#5fafaf"
	\, "#5fafd7"
	\, "#5fafff"
	\, "#5fd700"
	\, "#5fd75f"
	\, "#5fd787"
	\, "#5fd7af"
	\, "#5fd7d7"
	\, "#5fd7ff"
	\, "#5fff00"
	\, "#5fff5f"
	\, "#5fff87"
	\, "#5fffaf"
	\, "#5fffd7"
	\, "#5fffff"
	\, "#870000"
	\, "#87005f"
	\, "#870087"
	\, "#8700af"
	\, "#8700d7"
	\, "#8700ff"
	\, "#875f00"
	\, "#875f5f"
	\, "#875f87"
	\, "#875faf"
	\, "#875fd7"
	\, "#875fff"
	\, "#878700"
	\, "#87875f"
	\, "#878787"
	\, "#8787af"
	\, "#8787d7"
	\, "#8787ff"
	\, "#87af00"
	\, "#87af5f"
	\, "#87af87"
	\, "#87afaf"
	\, "#87afd7"
	\, "#87afff"
	\, "#87d700"
	\, "#87d75f"
	\, "#87d787"
	\, "#87d7af"
	\, "#87d7d7"
	\, "#87d7ff"
	\, "#87ff00"
	\, "#87ff5f"
	\, "#87ff87"
	\, "#87ffaf"
	\, "#87ffd7"
	\, "#87ffff"
	\, "#af0000"
	\, "#af005f"
	\, "#af0087"
	\, "#af00af"
	\, "#af00d7"
	\, "#af00ff"
	\, "#af5f00"
	\, "#af5f5f"
	\, "#af5f87"
	\, "#af5faf"
	\, "#af5fd7"
	\, "#af5fff"
	\, "#af8700"
	\, "#af875f"
	\, "#af8787"
	\, "#af87af"
	\, "#af87d7"
	\, "#af87ff"
	\, "#afaf00"
	\, "#afaf5f"
	\, "#afaf87"
	\, "#afafaf"
	\, "#afafd7"
	\, "#afafff"
	\, "#afd700"
	\, "#afd75f"
	\, "#afd787"
	\, "#afd7af"
	\, "#afd7d7"
	\, "#afd7ff"
	\, "#afff00"
	\, "#afff5f"
	\, "#afff87"
	\, "#afffaf"
	\, "#afffd7"
	\, "#afffff"
	\, "#d70000"
	\, "#d7005f"
	\, "#d70087"
	\, "#d700af"
	\, "#d700d7"
	\, "#d700ff"
	\, "#d75f00"
	\, "#d75f5f"
	\, "#d75f87"
	\, "#d75faf"
	\, "#d75fd7"
	\, "#d75fff"
	\, "#d78700"
	\, "#d7875f"
	\, "#d78787"
	\, "#d787af"
	\, "#d787d7"
	\, "#d787ff"
	\, "#d7af00"
	\, "#d7af5f"
	\, "#d7af87"
	\, "#d7afaf"
	\, "#d7afd7"
	\, "#d7afff"
	\, "#d7d700"
	\, "#d7d75f"
	\, "#d7d787"
	\, "#d7d7af"
	\, "#d7d7d7"
	\, "#d7d7ff"
	\, "#d7ff00"
	\, "#d7ff5f"
	\, "#d7ff87"
	\, "#d7ffaf"
	\, "#d7ffd7"
	\, "#d7ffff"
	\, "#ff0000"
	\, "#ff005f"
	\, "#ff0087"
	\, "#ff00af"
	\, "#ff00d7"
	\, "#ff00ff"
	\, "#ff5f00"
	\, "#ff5f5f"
	\, "#ff5f87"
	\, "#ff5faf"
	\, "#ff5fd7"
	\, "#ff5fff"
	\, "#ff8700"
	\, "#ff875f"
	\, "#ff8787"
	\, "#ff87af"
	\, "#ff87d7"
	\, "#ff87ff"
	\, "#ffaf00"
	\, "#ffaf5f"
	\, "#ffaf87"
	\, "#ffafaf"
	\, "#ffafd7"
	\, "#ffafff"
	\, "#ffd700"
	\, "#ffd75f"
	\, "#ffd787"
	\, "#ffd7af"
	\, "#ffd7d7"
	\, "#ffd7ff"
	\, "#ffff00"
	\, "#ffff5f"
	\, "#ffff87"
	\, "#ffffaf"
	\, "#ffffd7"
	\, "#ffffff"
	\, "#080808"
	\, "#121212"
	\, "#1c1c1c"
	\, "#262626"
	\, "#303030"
	\, "#3a3a3a"
	\, "#444444"
	\, "#4e4e4e"
	\, "#585858"
	\, "#626262"
	\, "#6c6c6c"
	\, "#767676"
	\, "#808080"
	\, "#8a8a8a"
	\, "#949494"
	\, "#9e9e9e"
	\, "#a8a8a8"
	\, "#b2b2b2"
	\, "#bcbcbc"
	\, "#c6c6c6"
	\, "#d0d0d0"
	\, "#dadada"
	\, "#e4e4e4"
	\, "#eeeeee"
	\]

function! s:urxvt_hi(hl, ...) abort
	exe "hi " . a:hl . " " . join(a:000)

	" set guifg/guibg from ctermfg/ctermbg
	let fg = synIDattr(hlID(a:hl), "fg", "cterm")
	let bg = synIDattr(hlID(a:hl), "bg", "cterm")
	let bold = synIDattr(hlID(a:hl), "bold", "cterm")
	if bold && fg < 8
		let fg += 8
	endif
	let guifg = empty(fg) ? "NONE" : s:urxvt[str2nr(fg)]
	let guibg = empty(bg) ? "NONE" : s:urxvt[str2nr(bg)]
	exe "hi " . a:hl . " guifg=" . guifg . " guibg=" . guibg
endfunction

command! -nargs=* HiU call s:urxvt_hi(<f-args>)

if &t_Co >= 256 || has('gui_running')
	hi Comment cterm=bold gui=bold
	hi Constant cterm=bold gui=bold
	hi CursorLine cterm=underline gui=underline guibg=NONE
	hi CursorLineNr cterm=bold gui=bold
	hi DiffDelete gui=NONE
	hi Folded cterm=bold gui=bold
	hi Identifier cterm=bold gui=bold
	hi MatchParen cterm=bold gui=bold
	hi MoreMsg gui=NONE
	hi NonText cterm=bold gui=bold
	hi PmenuSel cterm=reverse gui=reverse
	hi PreProc cterm=bold gui=bold
	hi Question gui=NONE
	hi Search cterm=underline gui=underline
	hi Special cterm=bold gui=bold
	hi SpecialKey cterm=bold gui=bold
	hi Statement cterm=bold gui=bold
	hi Title cterm=bold gui=bold
	hi Type cterm=bold gui=bold
	hi Underlined cterm=bold gui=bold
	hi VertSplit cterm=NONE gui=NONE
	hi link FloatBorder Pmenu
	hi link Whitespace SpecialKey

	if &bg == "dark"
		hi Normal ctermfg=15 guifg=white guibg=black

		HiU ColorColumn ctermbg=235
		HiU Comment ctermfg=45
		HiU Conceal ctermbg=242
		HiU Constant ctermfg=207
		HiU CursorLineNr ctermfg=250
		HiU DiffAdd ctermbg=4
		HiU DiffChange ctermbg=5
		HiU DiffDelete ctermbg=6 ctermfg=12
		HiU Directory ctermfg=159
		HiU EndOfBuffer ctermfg=238
		HiU ErrorMsg ctermbg=1
		HiU Folded ctermfg=11 ctermbg=240
		HiU Identifier ctermfg=14
		HiU LineNr ctermfg=246
		HiU MatchParen ctermbg=30
		HiU MoreMsg ctermfg=121
		HiU NonText ctermfg=202
		HiU Pmenu ctermbg=236 ctermfg=15
		HiU PmenuSel ctermbg=236 ctermfg=15
		HiU PreProc ctermfg=81
		HiU Question ctermfg=121
		HiU Search ctermbg=87 ctermfg=0
		HiU SignColumn ctermbg=233
		HiU Special ctermfg=208
		HiU SpecialKey ctermfg=239
		HiU Statement ctermfg=11
		HiU Title ctermfg=13
		HiU Type ctermfg=2
		HiU Underlined ctermfg=81
		HiU VertSplit ctermbg=250 ctermfg=232
		HiU Visual ctermbg=238
		HiU WarningMsg ctermfg=9
		HiU DiagnosticHint ctermfg=242
		HiU DiagnosticOk ctermfg=40
		HiU DiagnosticInfo ctermfg=33
		HiU DiagnosticWarn ctermfg=184
		HiU DiagnosticError ctermfg=160

		" cterm only:
		hi SpellBad cterm=underline,bold ctermfg=9 ctermbg=NONE
		hi SpellCap cterm=underline,bold ctermfg=87 ctermbg=NONE
		hi SpellLocal cterm=underline,bold ctermfg=39 ctermbg=NONE
		hi SpellRare cterm=underline,bold ctermfg=129 ctermbg=NONE
	else
		hi Normal guifg=black guibg=white

		" TODO
	endif

	" prevent vim-gitgutter from overriding diff colors
	hi def link diffAdded Identifier
	hi def link diffChanged PreProc
	hi def link diffRemoved Special

	" nvim diagnostics like ALE signs
	hi link DiagnosticSignError Error
	hi link DiagnosticSignWarn Todo
	hi link DiagnosticSignInfo Todo
	hi link DiagnosticSignHint Todo

	if has('nvim')
		hi link @punctuation NONE
		hi link @punctuation.bracket Special

		hi link @text.todo.checked Special
		hi link @text.todo.unchecked Special

		hi link @variable NONE
		hi link @lsp.type.variable NONE

		hi link @lsp.mod.attribute PreProc

		hi link @string.yaml NONE
	endif

	" fzf-lua colors
	hi def link FzfLuaFzfBorder SpecialKey
	hi def link FzfLuaFzfCursorLine CursorColumn
	hi def link FzfLuaFzfHeader Comment
	hi def link FzfLuaFzfInfo PreProc
	hi def link FzfLuaFzfPointer Exception
	hi def link FzfLuaFzfPrompt Conditional
endif

delcommand HiU
