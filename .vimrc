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
		exe "set <F21>=\<Esc>[5^"
		exe "set <F22>=\<Esc>[6^"
		nmap <silent> <F21> <C-PageUp>
		nmap <silent> <F22> <C-PageDown>

		" fix keycodes {{{4
		exe "set <S-Up>=\<Esc>[a"
		exe "set <S-Down>=\<Esc>[b"
		exe "set <C-Left>=\<Esc>Od"
		exe "set <C-Right>=\<Esc>Oc"
		exe "set <A-n>=\<Esc>n"
		exe "set <F14>=\<Esc>[26;*~"
		exe "set <F15>=\<Esc>[28;*~"
		exe "set <F16>=\<Esc>[29;*~"
		exe "set <F17>=\<Esc>[31;*~"
		exe "set <F18>=\<Esc>[32;*~"
		exe "set <F19>=\<Esc>[33;*~"
		exe "set <F20>=\<Esc>[34;*~"
		exe "set <Undo>="
		exe "set <Help>="
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

		" ctrl+pgup/down to switch tabs {{{4
		exe "set <F21>=\<Esc>[5;5~"
		exe "set <F22>=\<Esc>[6;5~"
		nmap <silent> <F21> <C-PageUp>
		nmap <silent> <F22> <C-PageDown>

		" fix keycodes {{{4
		exe "set <A-n>=\<Esc>n"
		exe "set <F13>=\<Esc>[1;2P"
		exe "set <F14>=\<Esc>[1;2Q"
		exe "set <F15>=\<Esc>[1;2R"
		exe "set <F16>=\<Esc>[1;2S"
		exe "set <F17>=\<Esc>[15;2~"
		exe "set <F18>=\<Esc>[17;2~"
		exe "set <F19>=\<Esc>[18;2~"
		exe "set <F20>=\<Esc>[19;2~"
	endif
else " {{{3
	" cursor shape {{{4
	set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-blinkoff500-blinkon500-Cursor,r-cr-o:hor20-blinkoff500-blinkon500-Cursor

	" ctrl+pgup/down to switch tabs {{{4
	nmap <silent> <F21> <C-PageUp>
	nmap <silent> <F22> <C-PageDown>
endif

" rxvt background switching {{{3
if &term == "rxvt-unicode-256color" " {{{3
	function! s:rxvt_bg() abort
		if &bg == "light"
			let $COLORFGBG = '0;default;15'
			if has('nvim')
				call chansend(v:stderr, "\33]10;black\7\33]11;white\7\33]12;black\7\33]708;white\7")
			else
				call echoraw("\33]10;black\7\33]11;white\7\33]708;white\7")
			endif
		else
			let $COLORFGBG = '15;default;0'
			if has('nvim')
				call chansend(v:stderr, "\33]10;white\7\33]11;black\7\33]12;white\7\33]708;black\7")
			else
				call echoraw("\33]10;white\7\33]11;black\7\33]708;black\7")
			endif
		endif
	endfunction
	augroup RxvtBg
		autocmd!
		autocmd OptionSet background call s:rxvt_bg()
	augroup END
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
if has('nvim')
	" emulate clipboard=autoselect
	vmap <LeftRelease> "*ygv
	vmap <2-LeftRelease> "*ygv
	vmap <3-LeftRelease> "*ygv
endif

" set terminal title regardless of whether it can be restored
set title

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
set cursorlineopt=number
set fillchars=vert:│,fold:-
set list
set listchars=tab:>\ ,trail:#,extends:→,precedes:←,nbsp:␣
set noshowmode
set shortmess+=I " no intro
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
set shortmess-=S " show number of matches

" location of tags, includes {{{2
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

" disable 'E325: ATTENTION Found a swap file…' {{{2
" (I'm a 90s kid, I save manually by pressing F2 every 2 seconds, I don't need
" vim to warn me about other vim instances.)
set directory=

" other {{{2
set autoread
set backspace=indent,eol,start
set completeopt=menu,menuone,longest
if !has('nvim')
	" see float_preview.nvim for the nvim version
	set completeopt+=popup
endif
set diffopt+=indent-heuristic,algorithm:histogram
if has('nvim')
	set diffopt+=linematch:30
endif
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

" LSP {{{2
sign define DiagnosticSignError text=>> texthl=DiagnosticSignError
sign define DiagnosticSignWarn text=-- texthl=DiagnosticSignWarn
sign define DiagnosticSignInfo text=-- texthl=DiagnosticSignInfo
sign define DiagnosticSignHint text=-- texthl=DiagnosticSignHint

let g:lsp_settings_elixirls = {}
let g:lsp_settings_elixirls['elixirLS'] = #{dialyzerEnabled: v:false}
let g:lsp_cmd_elixirls = [$HOME .. "/src-elixir/.build/elixir-ls/language_server.sh"]

let g:lsp_settings_pylsp = #{pylsp: #{plugins: {} } }
let g:lsp_settings_pylsp['pylsp']['configurationSources'] = ['flake8']

" disable mypy by default to prevent .mypy_cache appearing all over the filesystem
let g:lsp_settings_pylsp['pylsp']['plugins']['pylsp_mypy'] = #{enabled: v:false}

" use flake8 (covers functionality of pyflakes, pycodestyle, mccabe)
let g:lsp_settings_pylsp['pylsp']['plugins']['flake8'] = #{enabled: v:true}
let g:lsp_settings_pylsp['pylsp']['plugins']['pyflakes'] = #{enabled: v:false}
let g:lsp_settings_pylsp['pylsp']['plugins']['pycodestyle'] = #{enabled: v:false}
let g:lsp_settings_pylsp['pylsp']['plugins']['mccabe'] = #{enabled: v:false}

" disable black (formatting) and ruff (formatting + linting) by default, only enable isort
let g:lsp_settings_pylsp['pylsp']['plugins']['pyls_isort'] = #{enabled: v:true}
let g:lsp_settings_pylsp['pylsp']['plugins']['black'] = #{enabled: v:false}
let g:lsp_settings_pylsp['pylsp']['plugins']['ruff'] = #{enabled: v:false}

let g:lsp_settings_hls = #{haskell: #{plugin: {} } }
let g:lsp_settings_hls['haskell']['plugin']['stan'] = #{globalOn: v:false}

let g:lsp_settings_rust_analyzer = #{rust-analyzer: {}}
let g:lsp_settings_rust_analyzer['rust-analyzer']['cargo'] = {}
let g:lsp_settings_rust_analyzer['rust-analyzer']['cargo']['features'] = 'all'
let g:lsp_settings_rust_analyzer['rust-analyzer']['cargo']['allTargets'] = v:true

" See .config/nvim/lua/init/after_lsp.lua for neovim LSP configs
let g:lsp_autoformat_elixirls = v:true
let g:lsp_autostart_lua_ls = v:true
let g:lsp_autostart_pylsp = v:true
let g:lsp_autostart_tilt_ls = v:true
let g:lsp_null_enabled = {}
let g:lsp_null_enabled['hadolint'] = ['diagnostics']
let g:lsp_null_enabled['buildifier'] = ['diagnostics', 'formatting']
let g:lsp_null_enabled['proselint'] = ['code_actions', 'diagnostics']
let g:lsp_null_enabled['shellcheck'] = ['code_actions', 'diagnostics']
let g:lsp_null_settings = {}
let g:lsp_null_settings['proselint'] = #{filetypes: ["gitcommit", "mail", "markdown", "rst", "text"]}

let s:cargo_root = fnamemodify(findfile('Cargo.toml', fnameescape(getcwd()) . ';'), ':p:h')
if isdirectory(s:cargo_root . '/target/debug')
	" Enable only for projects that have been built at least once
	let g:lsp_autoformat_rust_analyzer = v:true
	let g:lsp_autostart_rust_analyzer = v:true
endif

let g:lsp_maximum_file_size = 131072

" fzf {{{2
if exists('$TMUX')
	let g:fzf_layout = { 'tmux': '-p90%,60%' }
else
	let g:fzf_layout = { 'down': '60%' }
endif
let g:fzf_colors = {
	\ 'fg':      ['fg', 'Normal'],
	\ 'bg':      ['bg', 'Normal'],
	\ 'hl':      ['fg', 'Special'],
	\ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
	\ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
	\ 'hl+':     ['fg', 'Special'],
	\ 'info':    ['fg', 'PreProc'],
	\ 'border':  ['fg', 'SpecialKey'],
	\ 'prompt':  ['fg', 'Conditional'],
	\ 'pointer': ['fg', 'Exception'],
	\ 'marker':  ['fg', 'Keyword'],
	\ 'spinner': ['fg', 'Label'],
	\ 'header':  ['fg', 'Comment']
\ }
let g:fzf_vim = {}
let g:fzf_vim.command_prefix = 'Fzf'

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
let g:UltiSnipsExpandTrigger = '<Plug>(ultisnips_expand)'
let g:UltiSnipsListSnippets = '<Plug>(ultisnips_list)'
let g:UltiSnipsJumpForwardTrigger = '<C-_>'
let g:UltiSnipsJumpBackwardTrigger = '<C-X><C-_>'
let g:snips_name = 'Tomáš Janoušek'
let g:snips_email = 'tomi@nomi.cz'
let g:snips_author = 'liskin'
let g:snips_github = "https://github.com/liskin"

" zepl {{{2
let g:repl_config = {}
let g:repl_config['python'] = #{cmd: 'ipython3', formatter: function('zepl#contrib#bracketedpaste#formatter')}

" old-style debian vim addons {{{2
set runtimepath+=/usr/share/vim/addons

" ft, syn {{{2
filetype plugin indent on
syntax on

" detect background colour, load my colourscheme {{{2
set bg&
colorscheme liskin " see ~/.vim/colors/liskin.vim

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
autocmd BufNewFile,BufRead /dev/shm/pass.* set viminfo= noswapfile noundofile et
autocmd BufNewFile,BufRead Jenkinsfile setf groovy
autocmd BufNewFile,BufRead */.config/git/include/* setf gitconfig

" disable LSP before it's attached to the buffer
autocmd FileType * if expand("<afile>:p") =~ glob2regpat("/dev/shm/pass.*") | let b:lsp_disabled = 1 | endif

" key maps {{{1

nnoremap <silent> <F1> <Cmd>:FzfLua helptags<CR>
inoremap <silent> <F1> <Cmd>:FzfLua helptags<CR>

nnoremap <silent> <F2> <Cmd>:w!<CR>
inoremap <silent> <F2> <Cmd>:w!<CR>

nnoremap <silent> <F3> <Cmd>:IndentGuidesToggle<CR>
inoremap <silent> <F3> <Cmd>:IndentGuidesToggle<CR>

nnoremap <silent> <F14> <Cmd>:cprevious<CR>
inoremap <silent> <F14> <Cmd>:cprevious<CR>
nnoremap <silent> <F4> <Cmd>:cnext<CR>
inoremap <silent> <F4> <Cmd>:cnext<CR>

nnoremap <silent> <F15> <Cmd>:lprevious<CR>
inoremap <silent> <F15> <Cmd>:lprevious<CR>
nnoremap <silent> <F5> <Cmd>:lnext<CR>
inoremap <silent> <F5> <Cmd>:lnext<CR>

if has('nvim')
	nnoremap <silent> <F6> <Cmd>:TroubleToggle<CR>
	inoremap <silent> <F6> <Cmd>:TroubleToggle<CR>

	nnoremap <silent> <F16> <Cmd>:LspLinesToggle<CR>
	inoremap <silent> <F16> <Cmd>:LspLinesToggle<CR>
endif

nnoremap <silent> <F8> <Cmd>:TagbarToggle<CR>
inoremap <silent> <F8> <Cmd>:TagbarToggle<CR>

if has('gui_running')
	nnoremap <silent> <F10> <Cmd>:ToggleMenu<CR>
	inoremap <silent> <F10> <Cmd>:ToggleMenu<CR>
	command ToggleMenu if &go=~'m'|set go-=m|else|set go+=m|endif
elseif has('nvim')
	nnoremap <silent> <F10> <Cmd>:FzfLua menus<CR>
endif

nnoremap <silent> <F11> <Cmd>:NERDTreeToggle<CR>
inoremap <silent> <F11> <Cmd>:NERDTreeToggle<CR>

" fzf keys {{{2
nnoremap <silent> <F12> <Cmd>:FzfLua builtin<CR>
nnoremap <silent> <C-B> <Cmd>:FzfLua buffers<CR>
nnoremap <silent> <C-G> <Cmd>:FzfLua tags<CR>
nnoremap <silent> <C-T> <Cmd>:FzfLua btags<CR>
nnoremap <silent> <C-P> <Cmd>:FzfLua files<CR>
nnoremap <silent> <C-Y> <Cmd>:FzfLua tabs<CR>
nnoremap <silent> <C-J> <Cmd>:FzfLua grep<CR>
nnoremap <silent> <C-X> <Cmd>:FzfCommands<CR>

" smart Tab completion (vim only) {{{2
if !has('nvim')
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
	function! LiskinShiftTabComplete() abort
		if pumvisible()
			return "\<C-P>"
		else
			return "\<Tab>"
		endif
	endfunction
	inoremap <silent> <Tab> <C-R>=LiskinTabComplete()<CR>
	inoremap <silent> <S-Tab> <C-R>=LiskinShiftTabComplete()<CR>
endif

" cloning/closing tabs {{{2
nnoremap <silent> <C-W>S <Cmd>:CloneBufTmp<CR>
nnoremap <silent> <C-W>C <Cmd>:tabclose<CR>

" git (fugitive) shortcuts {{{2
nnoremap <silent> <Leader>gg <Cmd>:0tab G<CR>
nnoremap <silent> <Leader>gv <Cmd>:Gvdiffsplit<CR>

" C-X is commands, remap decrement to C-Q {{{2
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

" hop keys {{{2
if has('nvim')
	noremap <silent> s <Cmd>:HopChar1<CR>
	noremap <silent> Sw <Cmd>:HopWord<CR>
	noremap <silent> Sl <Cmd>:HopLineStart<CR>
	noremap <silent> SL <Cmd>:HopLine<CR>
	noremap <silent> St <Cmd>:HopNodes<CR>
	noremap <silent> S/ <Cmd>:HopPattern<CR>
	noremap <silent> SS <Cmd>:HopAnywhere<CR>

	noremap <silent> Hs <Cmd>:HopChar1MW<CR>
	noremap <silent> Hw <Cmd>:HopWordMW<CR>
	noremap <silent> Hl <Cmd>:HopLineStartMW<CR>
	noremap <silent> HL <Cmd>:HopLineMW<CR>
	noremap <silent> Ht <Cmd>:HopNodesMW<CR>
	noremap <silent> H/ <Cmd>:HopPatternMW<CR>
	noremap <silent> HH <Cmd>:HopAnywhereMW<CR>
endif

" vim:set foldenable foldmethod=marker: {{{1
