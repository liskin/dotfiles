" set options {{{1

set nocompatible

" term tweaks {{{2

function! s:focus(focus)
	if a:focus
		checktime
		silent doautocmd <nomodeline> FocusGained
	else
		silent doautocmd <nomodeline> FocusLost
	endif
endfunction

for fn in range(13, 24)
	exe "noremap <silent> <F" . fn . "> <Nop>"
	exe "noremap! <silent> <F" . fn . "> <Nop>"
endfor

if &term == "rxvt-unicode-256color" " {{{3
	" my urxvt supports sgr mouse reporting {{{4
	set ttymouse=sgr

	" focus events
	let &t_ti = &t_ti . "\<Esc>[?1004h"
	let &t_te = "\<Esc>[?1004l" . &t_te
	exe "set <F15>=\<Esc>[O"
	exe "set <F16>=\<Esc>[I"
	nnoremap <silent> <F15> :call <SID>focus(0)<CR>
	nnoremap <silent> <F16> :call <SID>focus(1)<CR>

	" cursor shape {{{4
	let &t_SI = "\<Esc>[5 q"
	let &t_SR = "\<Esc>[3 q"
	let &t_EI = "\<Esc>[2 q"

	" ctrl+pgup/down to switch tabs {{{4
	exe "set <F13>=\<Esc>[5^"
	exe "set <F14>=\<Esc>[6^"
	nmap <silent> <F13> :tabprev<CR>
	nmap <silent> <F14> :tabnext<CR>

	" fix keycodes {{{4
	exe "set <S-Up>=\<Esc>[a"
	exe "set <S-Down>=\<Esc>[b"
	exe "set <C-Left>=\<Esc>Od"
	exe "set <C-Right>=\<Esc>Oc"
	exe "set <A-n>=\<Esc>n"
endif

if &term == "tmux-256color" " {{{3
	" tmux supports sgr mouse reporting {{{4
	set ttymouse=sgr

	" focus events
	let &t_ti = &t_ti . "\<Esc>[?1004h"
	let &t_te = "\<Esc>[?1004l" . &t_te
	exe "set <F15>=\<Esc>[O"
	exe "set <F16>=\<Esc>[I"
	nnoremap <silent> <F15> :call <SID>focus(0)<CR>
	nnoremap <silent> <F16> :call <SID>focus(1)<CR>

	" cursor shape {{{4
	let &t_SI = "\<Esc>[5 q"
	let &t_SR = "\<Esc>[3 q"
	let &t_EI = "\<Esc>[2 q"

	" bracketed paste {{{4
	let &t_BE="\<Esc>[?2004h"
	let &t_BD="\<Esc>[?2004l"
	let &t_PS="\<Esc>[200~"
	let &t_PE="\<Esc>[201~"

	" ctrl+pgup/down to switch tabs {{{4
	exe "set <F13>=\<Esc>[5;5~"
	exe "set <F14>=\<Esc>[6;5~"
	nmap <silent> <F13> :tabprev<CR>
	nmap <silent> <F14> :tabnext<CR>

	" fix keycodes {{{4
	exe "set <A-n>=\<Esc>n"
endif

" fix C-Space mappings in terminal {{{3
map <C-@> <C-Space>

" terminal timeouts, etc. {{{3
set lazyredraw
set notimeout
set ttimeout
set ttimeoutlen=50
set ttyfast

set mouse=a

if has('gui_running') " {{{2
	hi Normal guifg=white guibg=black
	set guifont=Fixed\ 10

	" make it look like no-gui vim {{{3
	set guioptions-=m " no menu
	set guioptions-=T " no toolbar
	set guioptions-=r " no right scrollbar
	set guioptions-=L " no left scrollbar
endif

" indent {{{2
set nocopyindent
set noautoindent
set nopreserveindent
set nojoinspaces
set shiftwidth=4
set smarttab
set softtabstop=-1 " use shiftwidth
set tabstop=4

" line wrap {{{2
set breakat=\ \	!@*-+;:,./?_
set breakindent
set breakindentopt=sbr
set linebreak
set showbreak=\\
set wrap

" visuals {{{2
set fillchars=vert:│,fold:-
set list
set listchars=tab:>\ ,trail:#,extends:→,precedes:←,nbsp:␣
set noshowmode
set showcmd

" keep windows equally tiled {{{2
set equalalways
autocmd VimResized * exe "normal \<C-W>="

" search {{{2
set hlsearch
set incsearch

" fsync / swap {{{2
set dir=~/.vim/swap//
set nofsync
set swapsync=

" location of tags, includes {{{2
set tags+=./tags;
set path=.,,

" filename completion {{{2
set wildmode=list:longest
set wildignore+=*.o,*.d,*.hi,*.beam,*.p_o,*.p_hi,*.pyc
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

" :grep {{{2
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m,%f:%l:%m

" other {{{2
set backspace=indent,eol,start
set completeopt=menu,menuone,longest,popup
set fileencodings=ucs-bom,utf-8,iso-8859-2
set foldlevelstart=99
set hidden
set omnifunc=ale#completion#OmniFunc
set scrolloff=10
set sessionoptions-=options
set spelllang=en
set splitbelow
set splitright

" plugins {{{1

let mapleader = ","
let maplocalleader = "\\"

let g:NERDDefaultAlign = "left"
let g:PHP_default_indenting = 1
let g:fugitive_gitlab_domains = ['https://gitlab.freedesktop.org', 'https://salsa.debian.org']
let g:gitgutter_enabled = 0
let g:gitgutter_override_sign_column_highlight = 0
let g:markdown_folding = 1
let g:netrw_browsex_viewer = "sensible-browser"
let g:netrw_http_cmd  = "curl"
let g:netrw_http_xcmd = "-LSsf -o"
let g:puppet_align_hashes = 0
let g:scala_scaladoc_indent = 1
let g:tex_comment_nospell = 1

" tagbar {{{2
let g:tagbar_autofocus=1
let g:tagbar_ctags_bin = "ctags"
let g:tagbar_left=1
let g:tagbar_type_elixir = {
	\ 'ctagstype' : 'Elixir',
	\ 'kinds' : [
		\ 'p:protocols',
		\ 'm:modules',
		\ 'e:exceptions',
		\ 'y:types',
		\ 'd:delegates',
		\ 'f:functions',
		\ 'c:callbacks',
		\ 'a:macros',
		\ 't:tests',
		\ 'i:implementations',
		\ 'o:operators',
		\ 'r:records'
	\ ],
	\ 'sro' : '.',
	\ 'kind2scope' : {
		\ 'p' : 'protocol',
		\ 'm' : 'module'
	\ },
	\ 'sort' : 0
\ }
let g:tagbar_type_markdown = {
	\ 'ctagstype' : 'markdown',
	\ 'ctagsbin' : 'markdown2ctags',
	\ 'ctagsargs' : '-f - --sort=yes --sro=##',
	\ 'kinds' : [
		\ 's:sections',
		\ 'i:images'
	\ ],
	\ 'sro' : '##',
	\ 'kind2scope' : {
		\ 's' : 'section'
	\ },
	\ 'sort' : 0
\ }

" airline {{{2
let g:airline_highlighting_cache = 1
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
let g:airline_symbols_ascii = 1
let g:airline_symbols = {}
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.linenr = ':'
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tagbar#enabled = 0
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#tabline#enabled = 0
function! s:AirlineInit()
	call airline#parts#define_raw('fileshorten', '%n:%{pathshorten(bufname("%"))}%m')
	let spc = g:airline_symbols.space
	let g:airline_section_c = airline#section#create(['%<', 'fileshorten', spc, 'readonly'])
	let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%3p%%'.spc, 'linenr', 'maxlinenr', spc.': %c%V'])
endfunction
autocmd User AirlineAfterInit call s:AirlineInit()

" async lint engine {{{2
let g:ale_completion_enabled = 0
let g:ale_fix_on_save = 1
let g:ale_fixers = {}
let g:ale_fixers['elixir'] = ['mix_format']
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_text_changed = 0
let g:ale_linter_aliases = {}
let g:ale_linter_aliases['gitcommit'] = ['mail']
let g:ale_linters = {}
let g:ale_linters['elixir'] = []
let g:ale_linters['gitcommit'] = ['proselint']
let g:ale_linters['mail'] = ['proselint']
let g:ale_linters['markdown'] = ['proselint']
let g:ale_linters['python'] = ['flake8', 'pyls']
let g:ale_linters['rst'] = ['proselint']
let g:ale_linters['sh'] = ['shellcheck']
let g:ale_linters['text'] = ['proselint']
"let g:ale_linters['yaml'] = ['yamllint']
let g:ale_linters_explicit = 1
let g:ale_maximum_file_size = 524288
let g:ale_set_highlights = 0

let g:ale_elixir_elixir_ls_config = {
	\ 'elixirLS': {
		\ 'dialyzerEnabled': v:false,
	\ }
\ }
let g:ale_elixir_elixir_ls_release = $HOME."/src-elixir/elixir-ls/rel"

let g:ale_c_build_dir_names = ['_build', 'build', 'bin']

function! s:ale_add_linter(ale_linters, filetype, linter) abort
	if !has_key(a:ale_linters, a:filetype)
		let a:ale_linters[a:filetype] = []
	endif
	call add(a:ale_linters[a:filetype], a:linter)
endfunction

function! s:ale_enable_linter(filetype, linter) abort
	if !exists('b:ale_linters')
		let b:ale_linters = deepcopy(g:ale_linters)
	endif
	call s:ale_add_linter(b:ale_linters, a:filetype, a:linter)

	ALELint
endfunction

command! -nargs=1 -bar AleBufEnableLinter call s:ale_enable_linter(&ft, <q-args>)
command! -nargs=+ -bar AleAddLinter call s:ale_add_linter(g:ale_linters, <f-args>)

" fzf {{{2
let g:fzf_command_prefix = 'Fzf'
let g:fzf_colors = {
	\ 'fg':      ['fg', 'Normal'],
	\ 'bg':      ['bg', 'Normal'],
	\ 'hl':      ['fg', 'Special'],
	\ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
	\ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
	\ 'hl+':     ['fg', 'Special'],
	\ 'info':    ['fg', 'PreProc'],
	\ 'border':  ['fg', 'Ignore'],
	\ 'prompt':  ['fg', 'Conditional'],
	\ 'pointer': ['fg', 'Exception'],
	\ 'marker':  ['fg', 'Keyword'],
	\ 'spinner': ['fg', 'Label'],
	\ 'header':  ['fg', 'Comment']
\ }

" load everything: debian addons, pathogen, ft, syn {{{2
set runtimepath+=/usr/share/vim/addons
call pathogen#infect()

filetype plugin indent on
syntax on

" autocmds {{{1

" open quickfix windows automatically {{{2
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost l* nested lwindow

" autoclose preview windows {{{2
autocmd CompleteDone * if pumvisible() == 0 | pclose | endif

" when editing a file, always jump to the last cursor position {{{2
" see :help last-position-jump
autocmd BufReadPost *
	\ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
	\ |   exe "normal! g`\""
	\ | endif

" ft-specifics {{{2
autocmd FileType alex setlocal tw=78 et
autocmd FileType c setlocal tw=78 path=.,/usr/include,,
autocmd FileType cabal setlocal tw=78 et
autocmd FileType cpp setlocal tw=78 path=.,/usr/include,,
autocmd FileType dot setlocal ai
autocmd FileType erlang setlocal formatoptions-=t formatoptions+=crql suffixesadd+=.erl path+=**
autocmd FileType gitcommit setlocal tw=72
autocmd FileType happy setlocal tw=78 et
autocmd FileType haskell setlocal tw=78 et
autocmd FileType html setlocal indentkeys&
autocmd FileType lhaskell setlocal tw=78 ai et
autocmd FileType mail setlocal tw=78 ts=8 ai et | let &fenc = &enc
autocmd FileType markdown setlocal ai formatoptions=tcroqn2 comments=n:> tw=78 et
autocmd FileType meson setlocal et
autocmd FileType perl setlocal isfname-=- formatoptions-=t formatoptions+=crql
autocmd FileType php setlocal indentkeys&
autocmd FileType python setlocal tw=78 et
autocmd FileType rst setlocal tw=78
autocmd FileType sgml setlocal indentkeys&
autocmd FileType svn setlocal tw=78 et
autocmd FileType tex compiler tex
autocmd FileType tex setlocal ai tw=78
autocmd FileType tex setlocal comments=:%%,:%
autocmd FileType tex,lhaskell syn region texZone start="\\begin{[a-z]\+code}" end="\\end{[a-z]\+code}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{alltt}" end="\\end{alltt}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{minted}" end="\\end{minted}\|%stopzone\>"
autocmd FileType tex,lhaskell syn region texZone start="\\begin{ndoc}" end="\\end{ndoc}\|%stopzone\>"
autocmd FileType xhtml setlocal indentkeys&
autocmd FileType xml setlocal indentkeys&
autocmd BufNewFile,BufRead *.dve setlocal cin
autocmd BufNewFile,BufRead *.hsc setlocal ft=haskell
autocmd BufNewFile,BufRead *.mdve setlocal cin
autocmd BufNewFile,BufRead *.txt setlocal tw=78 ai
autocmd BufNewFile,BufRead CMakeLists.txt setlocal tw=0
autocmd BufNewFile,BufRead COMMIT_EDITMSG setlocal fo=tcq et spell spelllang=en
autocmd BufNewFile,BufRead PULLREQ_EDITMSG setlocal ft=gitcommit fo=tcq et spell spelllang=en
autocmd BufNewFile,BufRead darcs-record* setf svn
autocmd BufNewFile,BufRead neomutt-*-\w\+ setf mail
autocmd BufNewFile,BufRead svn-commit.tmp setf svn
autocmd BufNewFile,BufRead /dev/shm/pass.* set viminfo= noswapfile noundofile et | let b:ale_enabled = 0
autocmd BufNewFile,BufRead Jenkinsfile setf groovy

" key maps {{{1

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

" nmap <silent> <F8> :Vista!!<CR>
" imap <silent> <F8> <C-O>:Vista!!<CR>

" nmap <silent> <F9> :Vista ale<CR>
" imap <silent> <F9> <C-O>:Vista ale<CR>

if has('gui_running')
	nmap <silent> <F10> :ToggleMenu<CR>
	imap <silent> <F10> <C-O>:ToggleMenu<CR>
	command ToggleMenu if &go=~'m'|set go-=m|else|set go+=m|endif
endif

nmap <silent> <F11> :NERDTreeToggle<CR>
imap <silent> <F11> <C-O>:NERDTreeToggle<CR>

nmap <silent> <F12> :GitGutterToggle<CR>
imap <silent> <F12> <C-O>:GitGutterToggle<CR>

nmap <silent> <C-P> :FzfFiles<CR>
nmap <silent> <C-B> :FzfBuffers<CR>
nmap <silent> <C-G> :FzfTags<CR>
nmap <silent> <C-T> :FzfBTags<CR>
nmap <silent> <C-X> :FzfCommands<CR>
nmap <C-J> :FzfRg<space>

nmap <C-]> <Plug>(ale_tags_fallback_go_to_definition)
nmap <C-W><C-]> <Plug>(ale_tags_fallback_go_to_definition_in_split)
nmap <C-H> <Plug>(ale_hover)
imap <C-H> <C-\><C-O><C-H>

nmap <silent> <C-W>S :CloneBufTmp<CR>

nmap <silent> <C-W>C :tabclose<CR>

" prevent x from overriding what's in the clipboard. {{{2
noremap x "_x
noremap X "_X

" seamlessly treat visual lines as actual lines when moving around. {{{2
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk
inoremap <silent> <Down> <C-o>gj
inoremap <silent> <Up> <C-o>gk

" vim:set foldenable foldmethod=marker: {{{1
