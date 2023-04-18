" set options {{{1

set nocompatible

" term tweaks {{{2

for fn in range(13, 24) " {{{3
	exe "noremap <silent> <F" . fn . "> <Nop>"
	exe "noremap! <silent> <F" . fn . "> <Nop>"
endfor

if !has('nvim')
	if &term == "rxvt-unicode-256color" " {{{3
		" my urxvt supports sgr mouse reporting {{{4
		set ttymouse=sgr

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

		function! s:rxvt_bg() abort
			if &bg == "light"
				let $COLORFGBG = '0;default;15'
				call echoraw("\33]10;black\7\33]11;white\7\33]708;white\7")
			else
				let $COLORFGBG = '15;default;0'
				call echoraw("\33]10;white\7\33]11;black\7\33]708;black\7")
			endif
		endfunction
		augroup RxvtBg
			autocmd!
			autocmd OptionSet background call s:rxvt_bg()
		augroup END
	endif

	if &term == "tmux-256color" " {{{3
		" tmux supports sgr mouse reporting {{{4
		set ttymouse=sgr

		" cursor shape {{{4
		let &t_SI = "\<Esc>[5 q"
		let &t_SR = "\<Esc>[3 q"
		let &t_EI = "\<Esc>[2 q"

		" focus reporting {{{4
		let &t_fe = "\<Esc>[?1004h"
		let &t_fd = "\<Esc>[?1004l"

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
else
	" ctrl+pgup/down to switch tabs {{{4
	nmap <silent> <C-PageUp> :tabprev<CR>
	nmap <silent> <C-PageDown> :tabnext<CR>
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
set mousemodel=popup_setpos

if has('gui_running') " {{{2
	set guifont=Fixed

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
set breakindentopt=sbr,shift:2,list:-1
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
function s:equal_windows() abort
	let save_pos = getpos(".")
	let cur_tab = tabpagenr()
	tabdo set cmdheight=1 | wincmd =
	execute 'tabn ' . cur_tab
	call setpos('.', save_pos)
endfunction
autocmd VimResized * call s:equal_windows()

" search {{{2
set hlsearch
set incsearch

" location of swap, tags, includes {{{2
set dir=~/.vim/swap//
set path=.,,
set tags+=./tags;

" filename completion {{{2
set wildmenu
set wildmode=full:longest
set wildoptions=pum
if has('patch-8.2.4608')
	set wildoptions+=fuzzy
endif
set wildignore+=*.o,*.d,*.hi,*.beam,*.p_o,*.p_hi,*.pyc
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

" :grep {{{2
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m,%f:%l:%m

" other {{{2
set autoread
set backspace=indent,eol,start
set completeopt=menu,menuone,longest,preview
if has('textprop')
	set completeopt-=preview completeopt+=popup
endif
set diffopt+=indent-heuristic,algorithm:histogram
set fileencodings=ucs-bom,utf-8,iso-8859-2
set foldlevelstart=99
set formatoptions+=rj
set hidden
set scrolloff=10
set sessionoptions-=options
set spelllang=en
set splitbelow
set splitright
set viewoptions-=options

" plugins {{{1

let mapleader = ","
let maplocalleader = "\\"

let g:PHP_default_indenting = 1
let g:fugitive_gitlab_domains = ['https://gitlab.freedesktop.org', 'https://salsa.debian.org']
let g:fugitive_legacy_commands = 0
let g:gitgutter_enabled = 0
let g:gitgutter_override_sign_column_highlight = 0
let g:markdown_folding = 1
let g:netrw_browsex_viewer = "sensible-browser"
let g:netrw_http_cmd  = "curl"
let g:netrw_http_xcmd = "-LSsf -o"
let g:puppet_align_hashes = 0
let g:scala_scaladoc_indent = 1
let g:speeddating_no_mappings = 1
let g:tcomment_mapleader1 = ''
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
let g:airline#extensions#fugitiveline#enabled = 0
let g:airline#extensions#searchcount#enabled = 0
function! VimrcAirlineFileshorten()
	" other airline parts have their own width heuristics and usually all fit within 35 columns…
	let width_available = airline#util#winwidth() - 35
	let name = bufname("%")

	let short_name = name
	let shorten_len = 5
	while len(short_name) > width_available && shorten_len > 0
		let short_name = pathshorten(name, shorten_len)
		let shorten_len -= 1
	endwhile

	return short_name
endfunction
function! s:AirlineInit()
	call airline#parts#define_raw('fileshorten', '%n:%{VimrcAirlineFileshorten()}%m')
	let spc = g:airline_symbols.space
	let g:airline_section_c = airline#section#create(['%<', 'fileshorten', spc, 'readonly'])
	let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%3p%%'.spc, 'linenr', 'maxlinenr', spc.': %c%V'])
endfunction
autocmd User AirlineAfterInit call s:AirlineInit()

" async lint engine {{{2
let g:ale_completion_enabled = 0
let g:ale_fix_on_save = 1
let g:ale_floating_preview = 1
let g:ale_floating_window_border = repeat([''], 6)
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_text_changed = 0
let g:ale_linters_explicit = 1
let g:ale_maximum_file_size = 524288
let g:ale_popup_menu_enabled = 1
let g:ale_root = {}
let g:ale_set_highlights = 0
let g:ale_virtualtext_cursor = 0

let g:ale_fixers = {}
let g:ale_linters = {}
let g:ale_linter_aliases = {}

function! s:ale_add_linter(ale_linters, filetype, linter) abort
	if !has_key(a:ale_linters, a:filetype)
		let a:ale_linters[a:filetype] = []
	endif

	if index(a:ale_linters[a:filetype], a:linter) == -1
		call add(a:ale_linters[a:filetype], a:linter)
	endif
endfunction

function! s:ale_enable_linter(var, filetype, linter) abort
	if !exists('b:{a:var}')
		let b:{a:var} = deepcopy(g:{a:var})
	endif
	call s:ale_add_linter(b:{a:var}, a:filetype, a:linter)

	ALELint
	ALEFix
endfunction

function! s:ale_add_linters(ale_linters, filetype, ...) abort
	for l:linter in a:000
		call s:ale_add_linter(a:ale_linters, a:filetype, l:linter)
	endfor
endfunction

command! -nargs=1 -bar AleBufEnableLinter call s:ale_enable_linter('ale_linters', &ft, <q-args>)
command! -nargs=1 -bar AleBufEnableFixer call s:ale_enable_linter('ale_fixers', &ft, <q-args>)
command! -nargs=+ -bar AleAddLinter call s:ale_add_linters(g:ale_linters, <f-args>)
command! -nargs=+ -bar AleAddFixer call s:ale_add_linters(g:ale_fixers, <f-args>)

let g:ale_elixir_elixir_ls_config = #{elixirLS: {}}
let g:ale_elixir_elixir_ls_config['elixirLS'] = #{dialyzerEnabled: v:false}
let g:ale_elixir_elixir_ls_release = $HOME."/src-elixir/.build/elixir-ls"

let g:ale_c_build_dir_names = ['_build', 'build', 'bin']

let g:ale_python_pylsp_config = #{pylsp: #{plugins: {}}}
let g:ale_python_pylsp_config['pylsp']['configurationSources'] = ['flake8']

" disable mypy by default to prevent .mypy_cache appearing all over the filesystem
let g:ale_python_pylsp_config['pylsp']['plugins']['pylsp_mypy'] = #{enabled: v:false}

" use flake8 (covers functionality of pyflakes, pycodestyle, mccabe)
let g:ale_python_pylsp_config['pylsp']['plugins']['flake8'] = #{enabled: v:true}
let g:ale_python_pylsp_config['pylsp']['plugins']['pyflakes'] = #{enabled: v:false}
let g:ale_python_pylsp_config['pylsp']['plugins']['pycodestyle'] = #{enabled: v:false}
let g:ale_python_pylsp_config['pylsp']['plugins']['mccabe'] = #{enabled: v:false}

let g:ale_haskell_ormolu_executable = 'fourmolu'
let g:ale_haskell_hls_config = #{haskell: #{plugin: {}}}
let g:ale_haskell_hls_config['haskell']['maxCompletions'] = 250
let g:ale_haskell_hls_config['haskell']['plugin']['stan'] = #{globalOn: v:false}

" Set a global project root for rust-analyzer to avoid starting a separate
" instance for dependencies. Assumes a single project, but that's how I use
" vim anyway.
let s:cargo_root = fnamemodify(findfile('Cargo.toml', fnameescape(getcwd()) . ';'), ':p:h')
let g:ale_root['analyzer'] = s:cargo_root
let g:ale_rust_analyzer_config = #{cargo: {}}
let g:ale_rust_analyzer_config['cargo'] = #{features: 'all'}
let g:ale_rust_rustfmt_options = '--edition 2021'

let g:ale_linter_aliases['gitcommit'] = ['mail']
AleAddFixer elixir mix_format
AleAddLinter dockerfile hadolint
AleAddLinter gitcommit proselint
AleAddLinter mail proselint
AleAddLinter markdown proselint
AleAddLinter python pylsp
AleAddLinter rst proselint
AleAddLinter sh shellcheck
AleAddLinter text proselint
AleAddLinter tilt buildifier
AleAddLinter tilt tilt_lsp

if isdirectory(s:cargo_root . '/target/debug')
	" Enable only for projects that have been built at least once
	AleAddLinter rust analyzer
	AleAddFixer rust rustfmt
endif

" fzf {{{2
if exists('$TMUX')
	let g:fzf_layout = { 'tmux': '-p90%,60%' }
else
	let g:fzf_prefer_height = 1
	let g:fzf_layout = { 'down': '60%' }
endif
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

" vimwiki {{{2
let g:taskwiki_disable_concealcursor = 1
let g:taskwiki_sort_order = 'status+,end+,due+,priority-'
let g:vimwiki_auto_chdir = 1
let g:vimwiki_autowriteall = 0
let g:vimwiki_conceal_pre = 1
let g:vimwiki_emoji_enable = 0
let g:vimwiki_folding = 'expr'
let g:vimwiki_global_ext = 0
let g:vimwiki_key_mappings = {}
let g:vimwiki_key_mappings['table_mappings'] = 0
let g:vimwiki_list = [{}]
let g:vimwiki_list[0].auto_tags = 1
let g:vimwiki_list[0].links_space_char = '-'
let g:vimwiki_list[0].listsym_rejected = 'D'
let g:vimwiki_list[0].listsyms = ' WSX'
let g:vimwiki_list[0].listsyms_propagate = 0
let g:vimwiki_list[0].name = 'taskwiki'
let g:vimwiki_list[0].path = '~/taskwiki'

" vim-visual-multi {{{2
let g:VM_maps = {}
let g:VM_maps["Add Cursor Down"] = ''
let g:VM_maps["Add Cursor Up"] = ''
let g:VM_maps["Select l"] = ''
let g:VM_maps["Select h"] = ''
let g:VM_mouse_mappings = 1

" vim-sleuth {{{2
let g:sleuth_gitcommit_heuristics = 0

" ultisnips {{{2
let g:UltiSnipsEditSplit = 'context'
let g:UltiSnipsExpandTrigger = '<c-x><c-_>'
let g:UltiSnipsListSnippets = ''
let g:snips_name = 'Tomáš Janoušek'
let g:snips_email = 'tomi@nomi.cz'
let g:snips_author = 'liskin'
let g:snips_github = "https://github.com/liskin"

" load plugins: debian addons, pathogen, ft, syn {{{2
set runtimepath+=/usr/share/vim/addons
let g:pathogen_disabled = []
call pathogen#infect()

filetype plugin indent on
syntax on

" detect background colour, load my colourscheme
set bg&
colors liskin

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
autocmd FileType gitcommit setlocal tw=72 et fo=tcq spell spelllang=en
autocmd FileType happy setlocal tw=78 et
autocmd FileType haskell setlocal tw=78 et
autocmd FileType html setlocal indentkeys&
autocmd FileType lhaskell setlocal tw=78 ai et
autocmd FileType markdown setlocal ai et tw=78
autocmd FileType meson setlocal et
autocmd FileType perl setlocal isfname-=- formatoptions-=t formatoptions+=crql
autocmd FileType php setlocal indentkeys&
autocmd FileType python setlocal tw=100 et
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
autocmd FileType text setlocal tw=78 ai
autocmd FileType xhtml setlocal indentkeys&
autocmd FileType xml setlocal indentkeys&
autocmd BufNewFile,BufRead *.hsc setlocal ft=haskell
autocmd BufNewFile,BufRead PULLREQ_EDITMSG setlocal ft=gitcommit
autocmd BufNewFile,BufRead /dev/shm/pass.* set viminfo= noswapfile noundofile et | let b:ale_enabled = 0
autocmd BufNewFile,BufRead Jenkinsfile setf groovy
autocmd BufNewFile,BufRead */.config/git/include/* setf gitconfig

" key maps {{{1

nnoremap <silent> <F2> <Cmd>:w!<CR>
inoremap <silent> <F2> <Cmd>:w!<CR>

nnoremap <silent> <F3> <Cmd>:IndentGuidesToggle<CR>
inoremap <silent> <F3> <Cmd>:IndentGuidesToggle<CR>

nnoremap <silent> <F4> <Cmd>:cprevious<CR>
inoremap <silent> <F4> <Cmd>:cprevious<CR>
nnoremap <silent> <F5> <Cmd>:cnext<CR>
inoremap <silent> <F5> <Cmd>:cnext<CR>

nnoremap <silent> <F6> <Cmd>:lprevious<CR>
inoremap <silent> <F6> <Cmd>:lprevious<CR>
nnoremap <silent> <F7> <Cmd>:lnext<CR>
inoremap <silent> <F7> <Cmd>:lnext<CR>

nnoremap <silent> <F8> <Cmd>:TagbarToggle<CR>
inoremap <silent> <F8> <Cmd>:TagbarToggle<CR>

if has('gui_running')
	nnoremap <silent> <F10> <Cmd>:ToggleMenu<CR>
	inoremap <silent> <F10> <Cmd>:ToggleMenu<CR>
	command ToggleMenu if &go=~'m'|set go-=m|else|set go+=m|endif
endif

nnoremap <silent> <F11> <Cmd>:NERDTreeToggle<CR>
inoremap <silent> <F11> <Cmd>:NERDTreeToggle<CR>

nnoremap <silent> <F12> <Cmd>:GitGutterToggle<CR>
inoremap <silent> <F12> <Cmd>:GitGutterToggle<CR>

nnoremap <silent> <C-P> <Cmd>:FzfFiles<CR>
nnoremap <silent> <C-B> <Cmd>:FzfBuffers<CR>
nnoremap <silent> <C-Y> <Cmd>:FzfWindows<CR>
nnoremap <silent> <C-G> <Cmd>:FzfTags<CR>
nnoremap <silent> <C-T> <Cmd>:FzfBTags<CR>
nnoremap <silent> <C-X> <Cmd>:FzfCommands<CR>

nnoremap <C-J> :FzfRg<space>

nnoremap <silent> <C-_> <Cmd>:FzfSnippets<CR>
inoremap <silent> <C-_> <Cmd>:FzfSnippets<CR>

nmap <C-]> <Plug>(ale_tags_fallback_go_to_definition)
nmap <C-W><C-]> <Plug>(ale_tags_fallback_go_to_definition_in_split)
nmap <C-H> <Plug>(ale_hover)
imap <C-H> <C-\><C-O><C-H>
nmap <C-K> <Plug>(ale_detail)
imap <C-K> <C-\><C-O><C-K>
nmap <C-F> <Plug>(ale_code_action)
imap <C-F> <C-\><C-O><C-F>

function! LiskinTabComplete() abort
	if pumvisible()
		return "\<C-N>"
	elseif strpart(getline('.'), 0, col('.') - 1) =~ '^\s*$'
		return "\<Tab>"
	elseif &omnifunc != ''
		return "\<C-X>\<C-O>"
	else
		return "\<C-N>"
	endif
endfunction
inoremap <silent> <Tab> <C-R>=LiskinTabComplete()<CR>
inoremap <silent> <S-Tab> <Tab>

nnoremap <silent> <C-W>S <Cmd>:CloneBufTmp<CR>
nnoremap <silent> <C-W>C <Cmd>:tabclose<CR>

" git (fugitive) shortcuts
nnoremap <silent> <Leader>gg <Cmd>:tab G<CR>
nnoremap <silent> <Leader>gv <Cmd>:Gvdiffsplit<CR>

" C-X is commands, remap decrement to C-Q
nnoremap <silent> <C-Q> <C-X>
vnoremap <silent> <C-Q> <C-X>

" prevent x from overriding what's in the clipboard. {{{2
noremap <silent> x "_x
noremap <silent> X "_X

" seamlessly treat visual lines as actual lines when moving around. {{{2
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk
inoremap <silent> <Down> <C-O>gj
inoremap <silent> <Up> <C-O>gk

" vim:set foldenable foldmethod=marker: {{{1
