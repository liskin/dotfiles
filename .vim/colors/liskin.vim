" Set 'background' back to the default.  The value can't always be estimated
" and is then guessed.
hi clear Normal
set bg&

" Remove all existing highlighting and set the defaults.
hi clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
	syntax reset
endif

let colors_name = "liskin"

if &t_Co >= 256 || has('gui_running')
	if &bg == "dark"
		hi ColorColumn ctermbg=235 guibg=#080808
		hi Comment cterm=bold ctermfg=45 gui=bold guifg=#00d7ff
		hi Conceal ctermbg=242 guibg=#6c6c6c
		hi Constant cterm=bold ctermfg=207 gui=bold guifg=#ff5fff
		hi CursorLine guibg=NONE gui=underline
		hi CursorLineNr cterm=bold ctermfg=250
		hi DiffAdd ctermbg=4 gui=NONE guibg=#0000c0
		hi DiffChange ctermbg=5 gui=NONE guibg=#c000c0
		hi DiffDelete ctermbg=6 ctermfg=12 gui=NONE guibg=#00c0c0 guifg=#0000ff
		hi Directory ctermfg=159 guifg=#afffff
		hi EndOfBuffer ctermfg=238 guifg=#444444
		hi ErrorMsg ctermbg=1 guibg=#c00000
		hi Folded cterm=bold ctermfg=11 ctermbg=240
		hi Identifier cterm=bold ctermfg=14 gui=bold guifg=#00ffff
		hi LineNr ctermfg=246
		hi MatchParen ctermbg=30 cterm=bold guibg=#008787 gui=bold
		hi MoreMsg gui=NONE ctermfg=121 guifg=#87ffaf
		hi NonText cterm=bold ctermfg=202 gui=bold guifg=#ff5f00
		hi Pmenu ctermbg=236 ctermfg=15 guibg=#303030 guifg=#ffffff
		hi PmenuSel cterm=reverse gui=reverse ctermbg=236 ctermfg=15 guibg=#303030 guifg=#ffffff
		hi PreProc cterm=bold ctermfg=81 gui=bold guifg=#5fd7ff
		hi Question gui=NONE ctermfg=121 guifg=#87ffaf
		hi Search cterm=underline ctermbg=87 ctermfg=0 gui=underline guibg=#5fffff guifg=black
		hi SignColumn ctermbg=233
		hi Special cterm=bold ctermfg=208 gui=bold guifg=#ff8700
		hi SpecialKey cterm=bold ctermfg=239 gui=NONE guifg=#4e4e4e
		hi link Whitespace SpecialKey
		hi SpellBad cterm=underline,bold ctermfg=9 ctermbg=NONE
		hi SpellCap cterm=underline,bold ctermfg=87 ctermbg=NONE
		hi SpellLocal cterm=underline,bold ctermfg=39 ctermbg=NONE
		hi SpellRare cterm=underline,bold ctermfg=129 ctermbg=NONE
		hi Statement cterm=bold ctermfg=11 gui=bold guifg=#ffff00
		hi Title ctermfg=13 cterm=bold
		hi Type cterm=bold ctermfg=2 gui=bold guifg=#00ff00
		hi Underlined cterm=bold ctermfg=81 gui=bold guifg=#5fd7ff
		hi VertSplit cterm=NONE gui=NONE ctermbg=250 ctermfg=232 guibg=#bcbcbc guifg=#080808
		hi Visual ctermbg=238 guibg=#444444
		hi WarningMsg ctermfg=9
		"hi TabLineFill term=underline cterm=underline gui=underline
		"hi TabLineSel ctermbg=4
	else
		" TODO
	endif

	" prevent vim-gitgutter from overriding diff colors
	hi def link diffAdded Identifier
	hi def link diffChanged PreProc
	hi def link diffRemoved Special
endif
