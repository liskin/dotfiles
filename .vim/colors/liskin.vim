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
	hi link Whitespace SpecialKey
	hi Statement cterm=bold gui=bold
	hi Title cterm=bold gui=bold
	hi Type cterm=bold gui=bold
	hi Underlined cterm=bold gui=bold
	hi VertSplit cterm=NONE gui=NONE

	if &bg == "dark"
		hi ColorColumn ctermbg=235 guibg=#080808
		hi Comment ctermfg=45 guifg=#00d7ff
		hi Conceal ctermbg=242 guibg=#6c6c6c
		hi Constant ctermfg=207 guifg=#ff5fff
		hi CursorLineNr ctermfg=250
		hi DiffAdd ctermbg=4 guibg=#0000c0
		hi DiffChange ctermbg=5 guibg=#c000c0
		hi DiffDelete ctermbg=6 ctermfg=12 guibg=#00c0c0 guifg=#0000ff
		hi Directory ctermfg=159 guifg=#afffff
		hi EndOfBuffer ctermfg=238 guifg=#444444
		hi ErrorMsg ctermbg=1 guibg=#c00000
		hi Folded ctermfg=11 ctermbg=240
		hi Identifier ctermfg=14 guifg=#00ffff
		hi LineNr ctermfg=246
		hi MatchParen ctermbg=30 guibg=#008787
		hi MoreMsg ctermfg=121 guifg=#87ffaf
		hi NonText ctermfg=202 guifg=#ff5f00
		hi Pmenu ctermbg=236 ctermfg=15 guibg=#303030 guifg=#ffffff
		hi PmenuSel ctermbg=236 ctermfg=15 guibg=#303030 guifg=#ffffff
		hi PreProc ctermfg=81 guifg=#5fd7ff
		hi Question ctermfg=121 guifg=#87ffaf
		hi Search ctermbg=87 ctermfg=0 guibg=#5fffff guifg=black
		hi SignColumn ctermbg=233
		hi Special ctermfg=208 guifg=#ff8700
		hi SpecialKey ctermfg=239 guifg=#4e4e4e
		hi Statement ctermfg=11 guifg=#ffff00
		hi Title ctermfg=13
		hi Type ctermfg=2 guifg=#00ff00
		hi Underlined ctermfg=81 guifg=#5fd7ff
		hi VertSplit ctermbg=250 ctermfg=232 guibg=#bcbcbc guifg=#080808
		hi Visual ctermbg=238 guibg=#444444
		hi WarningMsg ctermfg=9

		" cterm only:
		hi SpellBad cterm=underline,bold ctermfg=9 ctermbg=NONE
		hi SpellCap cterm=underline,bold ctermfg=87 ctermbg=NONE
		hi SpellLocal cterm=underline,bold ctermfg=39 ctermbg=NONE
		hi SpellRare cterm=underline,bold ctermfg=129 ctermbg=NONE
	else
		" TODO
	endif

	" prevent vim-gitgutter from overriding diff colors
	hi def link diffAdded Identifier
	hi def link diffChanged PreProc
	hi def link diffRemoved Special
endif
