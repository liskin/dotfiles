if &term == "rxvt-unicode-256color"
	" my urxvt supports sgr mouse reporting
	set ttymouse=sgr

	" cursor shape
	let &t_SI = "[5 q"
	let &t_SR = "[3 q"
	let &t_EI = "[2 q"

	" ctrl+pgup/down to switch tabs
	set <F13>=[5^
	set <F14>=[6^
	nmap <silent> <F13> :tabprev<CR>
	nmap <silent> <F14> :tabnext<CR>

	" fix keycodes
	set <S-Up>=[a
	set <S-Down>=[b
	set <C-Left>=Od
	set <C-Right>=Oc
	set <A-n>=n
endif

if &term == "tmux-256color"
	" tmux supports sgr mouse reporting
	set ttymouse=sgr

	" cursor shape
	let &t_SI = "[5 q"
	let &t_SR = "[3 q"
	let &t_EI = "[2 q"

	" bracketed paste
	let &t_BE="\<Esc>[?2004h"
	let &t_BD="\<Esc>[?2004l"
	let &t_PS="\<Esc>[200~"
	let &t_PE="\<Esc>[201~"

	" ctrl+pgup/down to switch tabs
	set <F13>=[5;5~
	set <F14>=[6;5~
	nmap <silent> <F13> :tabprev<CR>
	nmap <silent> <F14> :tabnext<CR>

	" fix keycodes
	set <A-n>=n
endif

if has('gui_running')
	hi Normal guifg=white guibg=black
	set guifont=Fixed\ 10

	" make it look like no-gui vim
	set guioptions-=m " no menu
	set guioptions-=T " no toolbar
	set guioptions-=r " no right scrollbar
	set guioptions-=L " no left scrollbar
endif

set nocompatible
set tabstop=4
set shiftwidth=4
set softtabstop=-1
set smarttab
"set expandtab
set copyindent
set noautoindent
set preserveindent
set backspace=2
set hidden
set incsearch
set mouse=a
set fileencodings=ucs-bom,utf-8,iso-8859-2
"set encoding=iso-8859-2
" let Tlist_Sort_Type = 'name'
let Tlist_Exit_OnlyWindow = 1
let tlist_tex_settings = 'latex;s:sections;g:graphics;l:labels'
let tlist_lhaskell_settings = 'latex;s:sections;g:graphics;l:labels'
set completeopt=menu,menuone,longest
"let g:doxygen_javadoc_autobrief = 0
set spelllang=en
let PHP_default_indenting = 1
set nofsync
set swapsync=
"set nofoldenable
set foldlevelstart=99
"set number
"set tags+=~/.vim/systags
set tags+=./tags;
set ru ruf=%40(%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P%)
"set listchars=eol:$,tab:>-,trail:~
set listchars=tab:>\ ,trail:#,extends:→,precedes:←,nbsp:␣
set list
set runtimepath+=/usr/share/vim/addons
let mapleader = ","
let maplocalleader = "\\"
set cscopequickfix=s-,c-,d-,i-,t-,e-
set cscopetag
set wildmode=list:longest
set hlsearch
set scrolloff=10
"let c_space_errors = 1
let g:haskell_indent_if = 4
let g:haskell_indent_case = 4
let g:netrw_http_cmd  = "wget"
let g:netrw_http_xcmd = "-q -O"
"let g:netrw_http_cmd = "elinks"
"let g:netrw_http_xcmd= "-source >"
let g:tex_comment_nospell = 1
set equalalways
let g:haddock_browser='surf'
let g:haddock_browser_callformat = '%s %s >/dev/null 2>&1 &'
let g:haddock_indexfiledir=$HOME.'/.vim/'
"let g:haddock_docdir=$HOME.'/.cabal/share/doc/'
autocmd VimResized * exe "normal \<C-W>="
set dir=~/.vim/swap//
let perl_include_pod=1
set laststatus=2
"let g:ycm_auto_trigger=0
"let g:ycm_key_invoke_completion = '<C-K>'
set wildignore+=*.o,*.d,*.hi,*.beam,*.p_o,*.p_hi,*.pyc
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
"set sessionoptions=blank,buffers,curdir,folds,help,tabpages,winsize
set sessionoptions-=options
set fillchars=vert:│,fold:-
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_working_path_mode = '0'
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_enabled = 0
let g:scala_scaladoc_indent = 1
let g:puppet_align_hashes = 0

set wrap
set showbreak=→
set breakindent
set breakindentopt=sbr
set linebreak
set breakat=\ \	!@*-+;:,./?_

set showcmd
set ttyfast
set notimeout
set ttimeout
set ttimeoutlen=50
set lazyredraw

"let g:airline_theme="wombat"
let g:airline_theme="dark"
let g:airline_theme_patch_func = 'AirlineThemePatch'
function! AirlineThemePatch(palette)
	if g:airline_theme == 'dark'
		for colors in values(a:palette.inactive)
			let colors[0] = '#080808'
			let colors[1] = '#bcbcbc'
			let colors[2] = 232
			let colors[3] = 250
		endfor
		for colors in values(a:palette.inactive_modified)
			let colors[0] = '#870000'
			let colors[2] = 88
		endfor
	endif
endfunction
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif
let g:airline_symbols_ascii = 1
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.linenr = ':'
let g:airline_left_sep = '▒'
let g:airline_right_sep = '▒'
"let g:airline#extensions#default#section_truncate_width = { 'b': 79, 'x': 80, 'y': 88, 'z': 45, }
let g:airline#extensions#tagbar#enabled = 0
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#tabline#enabled = 0
function! AirlineInit()
	call airline#parts#define_raw('fileshorten', '%n:%{pathshorten(bufname("%"))}%m')
	let spc = g:airline_symbols.space
	let g:airline_section_c = airline#section#create(['%<', 'fileshorten', spc, 'readonly'])
	let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%3p%%'.spc, 'linenr', 'maxlinenr', spc.': %c%V'])
endfunction
autocmd User AirlineAfterInit call AirlineInit()
set noshowmode

let g:tagbar_left=1
let g:tagbar_autofocus=1

"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
"let g:syntastic_haskell_checkers = ["ghc_modi"]

"let g:ghcmod_ghc_options = ['-Wwarn']

let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_set_highlights = 1
let g:ale_completion_enabled = 1
let g:ale_linters_explicit = 1
let g:ale_linters = {}
let g:ale_linter_aliases = {}
let g:ale_linters['python'] = ['flake8', 'mypy', 'pyls']
let g:ale_linters['sh'] = ['shellcheck']
let g:ale_linters['mail'] = ['proselint']
let g:ale_linters['text'] = ['proselint']
let g:ale_linters['rst'] = ['proselint']
let g:ale_linters['markdown'] = ['proselint']
let g:ale_linters['gitcommit'] = ['proselint']
let g:ale_linter_aliases['gitcommit'] = ['mail']
"let g:ale_linters['yaml'] = ['yamllint']

filetype plugin indent on
let mysyntaxfile="~/.vim/doxygen_load.vim"
syntax on

autocmd QuickFixCmdPost [^l]*grep* cwindow
autocmd QuickFixCmdPost l*grep* lwindow

autocmd FileType mail setlocal tw=78 ts=8 ai et | let &fenc = &enc | call RightMargin()
autocmd BufNewFile,BufRead *.txt setlocal tw=78 ai
autocmd BufNewFile,BufRead CMakeLists.txt setlocal tw=0
" autocmd FileType html setlocal tw=78
" autocmd FileType xhtml setlocal tw=78
" autocmd FileType php setlocal tw=78
autocmd FileType html call RightMargin()  | setlocal indentkeys&
autocmd FileType xhtml call RightMargin() | setlocal indentkeys&
autocmd FileType php call RightMargin()   | setlocal indentkeys&
autocmd FileType xml call RightMargin()   | setlocal indentkeys&
autocmd FileType sgml call RightMargin()  | setlocal indentkeys&
autocmd FileType svn setlocal tw=78 et | call RightMargin()
autocmd FileType make setlocal sw=8 nosta noet | call RightMargin()
autocmd FileType c setlocal tw=78 | call RightMargin()
autocmd FileType cpp setlocal tw=78 | call RightMargin()
"autocmd FileType tex setlocal fenc=iso-8859-2 ai tw=78 | call RightMargin()
autocmd FileType tex setlocal ai tw=78 | call RightMargin()
autocmd FileType tex compiler tex
autocmd FileType tex setlocal comments=:%%,:%
autocmd FileType tex,lhaskell syn region texZone start="\\begin{alltt}" end="\\end{alltt}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{minted}" end="\\end{minted}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{ndoc}" end="\\end{ndoc}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{[a-z]\+code}" end="\\end{[a-z]\+code}\|%stopzone\>"
autocmd FileType dot setlocal ai
autocmd FileType python setlocal tw=78 et | call RightMargin()
autocmd FileType perl setlocal isfname-=- formatoptions-=t formatoptions+=crql
"autocmd FileType erlang setlocal indentkeys=o,O formatoptions-=t formatoptions+=crql suffixesadd+=.erl path+=**
autocmd FileType erlang setlocal formatoptions-=t formatoptions+=crql suffixesadd+=.erl path+=**
autocmd FileType markdown setlocal ai formatoptions=tcroqn2 comments=n:> tw=78 et
autocmd FileType rst setlocal tw=78
"autocmd FileType votl call MyOutline()
autocmd FileType gitcommit setlocal tw=72
autocmd BufNewFile,BufRead *.dve setlocal cin
autocmd BufNewFile,BufRead *.mdve setlocal cin
autocmd BufNewFile,BufRead *.hsc setlocal ft=haskell
"autocmd BufNewFile,BufRead Makefile setlocal noet
autocmd BufNewFile,BufRead svn-commit.tmp setf svn
autocmd BufNewFile,BufRead darcs-record* setf svn
autocmd BufNewFile,BufRead COMMIT_EDITMSG setlocal fo=tcq et spell spelllang=en | call RightMargin()
autocmd BufNewFile,BufRead PULLREQ_EDITMSG setlocal ft=gitcommit fo=tcq et spell spelllang=en | call RightMargin()
autocmd BufNewFile,BufRead neomutt-*-\w\+ setf mail

" ----- Haskell ------
autocmd FileType lhaskell setlocal tw=78 ai et | call RightMargin() | call HaskellSyntax() | syntax sync fromstart
autocmd FileType haskell setlocal tw=78 et | call RightMargin() | call HaskellSyntax() | syntax sync fromstart
autocmd FileType alex setlocal tw=78 et | syntax sync fromstart
autocmd FileType happy setlocal tw=78 et | syntax sync fromstart
autocmd FileType cabal setlocal tw=78 et

" ----- Projekty -----
autocmd BufNewFile,BufRead */linux-2.6*/* setlocal sw=8 ts=8
autocmd BufNewFile,BufRead */linux-liskin*/* setlocal sw=8 ts=8
autocmd BufNewFile,BufRead */fluxbox*/* setlocal et "| cs add /usr/src/tomi/fluxbox/src/cscope.out
autocmd BufNewFile,BufRead */brutalis/* setlocal et
autocmd BufNewFile,BufRead /tmp/mozex.textarea*.txt setlocal noeol
autocmd BufNewFile,BufRead */divine*/* setlocal et
autocmd BufNewFile,BufRead */work/GoodData/{bear,aqe,gcf,puppet}/* setlocal ts=4 sw=4 tw=78 noet
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/* setlocal ts=4 sw=4 et | let g:is_bash = 1
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/cish/* setlocal ts=4 sw=2 tw=0 et
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/jenkins/scripts/* setlocal ts=4 sw=2 tw=78 et
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/testwatch/* setlocal noet
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/*.t setlocal ft=cram sw=2 tw=0 ai
autocmd BufNewFile,BufRead */work/GoodData/ci-infra*/*.sh setlocal sw=2 tw=0
autocmd BufNewFile,BufRead */work/GoodData/rolapps*/*.tt setlocal ft=perl
autocmd BufNewFile,BufRead *.pwt setlocal ft=perl
autocmd BufNewFile,BufRead ~/src-erlang/otp/* setlocal ts=8 sw=4
autocmd BufNewFile,BufRead ~/work/briskat/* setlocal et
autocmd BufNewFile,BufRead ~/work/SQLdep/* setlocal et
autocmd BufNewFile,BufRead ~/work/iXperta/ixcom/* setlocal et
autocmd BufNewFile,BufRead ~/work/iXperta/ixcom/*.yaml setlocal sw=2
autocmd BufNewFile,BufRead ~/work/iXperta/ixcom/*.yml setlocal sw=2
autocmd BufNewFile,BufRead ~/src-scala/* setlocal et
autocmd BufNewFile,BufRead ~/android/*.xml setlocal et sw=2
autocmd BufNewFile,BufRead ~/android/*.json setlocal et

autocmd FileType make setlocal noet


" skeletons
"autocmd BufNewFile *.c 0r ~/vim/skeleton/skel.c

" ----- Klavesnice -----
"imap <Up> <C-O>gk
"imap <Down> <C-O>gj
"nmap [5^ <C-PageUP>
"imap [5^ <C-PageUP>
"nmap [6^ <C-PageDown>
"imap [6^ <C-PageDown>
"nmap <silent> <F8> :TlistToggle<CR>
"imap <silent> <F8> <C-O>:TlistToggle<CR>
nmap <silent> <F2> :w!<CR>
imap <silent> <F2> <C-O>:w!<CR>

nmap <silent> <F3> :IndentGuidesToggle<CR>
imap <silent> <F3> <C-O>:IndentGuidesToggle<CR>

nmap <silent> <F4> :cprevious<CR>
imap <silent> <F4> <C-O>:cprevious<CR>
nmap <silent> <F5> :cnext<CR>
imap <silent> <F5> <C-O>:cnext<CR>

nmap <silent> <F6> :lprevious<CR>
imap <silent> <F6> <C-O>:lprevious<CR>
nmap <silent> <F7> :lnext<CR>
imap <silent> <F7> <C-O>:lnext<CR>

nmap <silent> <F8> :TagbarToggle<CR>
imap <silent> <F8> <C-O>:TagbarToggle<CR>

"nmap <silent> <F9> :make<CR>
"imap <silent> <F9> <C-O>:make<CR>

if has('gui_running')
	nmap <silent> <F10> :ToggleMenu<CR>
	imap <silent> <F10> <C-O>:ToggleMenu<CR>
	command ToggleMenu if &go=~'m'|set go-=m|else|set go+=m|endif
endif

nmap <silent> <F11> :NERDTreeToggle<CR>
imap <silent> <F11> <C-O>:NERDTreeToggle<CR>

nmap <silent> <F12> :GitGutterToggle<CR>
imap <silent> <F12> <C-O>:GitGutterToggle<CR>

nmap <silent> <C-B> :CtrlPBuffer<CR>
nmap <silent> <C-T> :CtrlPBufTag<CR>
nmap <silent> <C-G> :CtrlPTag<CR>

nmap <C-H> <Plug>(ale_hover)
imap <C-H> <C-\><C-O><C-H>
"imap <C-Space> <Plug>(ale_complete)

" Prevent x from overriding what's in the clipboard.
noremap x "_x
noremap X "_X

" Seamlessly treat visual lines as actual lines when moving around.
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk
" nnoremap <Down> gj
" nnoremap <Up> gk
" vnoremap <Down> gj
" vnoremap <Up> gk
inoremap <silent> <Down> <C-o>gj
inoremap <silent> <Up> <C-o>gk

" cmap Q q

" ----- Barvy -----
" see .vim/after/syntax/syncolor.vim

function ToHTML()
	runtime syntax/2html.vim
endfunction
function ToTeX()
	source ~/.vim/2tex.vim
endfunction

function RightMargin()
	highlight rightMargin ctermbg=magenta
	"2match rightMargin / \+\t\+\|^\s\+$/
	"2match rightMargin /\%>78v\| \+\t\+\|^\s\+$/
	"2match rightMargin /\%>78v\|^ \+\t\|\s\+$/
	" 79th column, spaces followed by tab, trailing whitespace
endfunction

function CscopeAuto(file)
	set nocsverb
	let l:fpath = fnamemodify(a:file, ":p:h")
	let l:csout = findfile("cscope.out", l:fpath . ";")
	if strlen(l:csout) > 0
		let l:cspath = fnamemodify(l:csout, ":p:h")
		exe "cs" "add" fnameescape(l:csout) fnameescape(l:cspath)
	elseif $CSCOPE_DB != ""
		cs add $CSCOPE_DB
	endif
	set csverb
endfunction
call CscopeAuto(".")
autocmd BufNewFile,BufRead * call CscopeAuto(expand("%"))

if strlen(glob("build.xml")) > 0
	compiler! ant
endif

" html shortcuts :)
"map <Leader>h1 a<h1></h1><c-o>4h
"map <Leader>h2 a<h2></h2><c-o>4h
"map <Leader>h3 a<h3></h3><c-o>4h

" tex shortcuts
"map <Leader>code o\begin{code}<esc>o\end{code}<esc>O
"map <Leader>ecode o\end{code}<esc>o\begin{code}<esc>O
"map <Leader>verb o\begin{verbatim}<esc>o\end{verbatim}<esc>O
"map <Leader>enum o\begin{enumerate}<esc>o\end{enumerate}<esc>O\item<space>
"map <Leader>item o\begin{itemize}<esc>o\end{itemize}<esc>O\item<space>
"map <Leader>frame o\begin{frame}<esc>o\end{frame}<esc>O<tab>\frametitle{}<c-o>i

" nbsp :-)
"map <Leader>nbsp :s/ /\="\<Char-0x00a0>"/g<CR>
"map <Leader>hsc :%! HsColour -css -lit<CR>

" reviewed by
map <Leader>rb oReviewed-by: <C-R>=ListReviewers()<CR>

func! ListReviewers()
	call complete( col('.'),
		\ [ 'Joe Example <joe@example.com>'
		\ ] )
	return ''
endfunc

function HaskellSyntax()
	syn match hsVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[∷⇒≡≢∀∃∪∩∨∧≥≤≠→←⊏⊐⊑⊒]"
	syn match hsTypedef "\<forall\>"

	map <Leader>gi :GhcModInfo<CR>
	map <Leader>gt :GhcModType<CR>
	map <Leader>gT :GhcModTypeClear<CR>
	"map <LocalLeader>w :GhcModTypeInsert<CR>
	"map <LocalLeader>s :GhcModSplitFunCase<CR>
	"map <LocalLeader>q :GhcModType<CR>
	"map <LocalLeader>e :GhcModTypeClear<CR>
endfunction

function MyOutline()
	if &term == "rxvt-256color" || &term == "screen.rxvt" || &term == "rxvt-unicode-256color"
		hi BT1 ctermfg=119
		hi BT2 ctermfg=119
		hi BT3 ctermfg=119
		hi BT4 ctermfg=119
		hi BT5 ctermfg=119
		hi BT6 ctermfg=119
		hi BT7 ctermfg=119
		hi BT8 ctermfg=119
		hi BT9 ctermfg=119
		hi OL1 ctermfg=230
		hi OL2 ctermfg=194
		hi OL3 ctermfg=158
		hi OL4 ctermfg=122
		hi OL5 ctermfg=230
		hi OL6 ctermfg=194
		hi OL7 ctermfg=158
		hi OL8 ctermfg=122
		hi OL9 ctermfg=230
	endif
endfunction

" When editing a file, always jump to the last cursor position
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal g'\"" |
\ endif

let g:save_session = 0

function SaveSession()
	if g:save_session
		"CMiniBufExplorer
		mksession! .vimsession
	endif
endfunc

function NewSession()
	let g:save_session = 1
	call SaveSession()
endfunc
command NewSession call NewSession()

autocmd VimLeave * call SaveSession()
autocmd SessionLoadPost * let g:save_session = 1

"let g:pathogen_blacklist = ["syntastic", "ghcmod-vim"]
call pathogen#infect()
