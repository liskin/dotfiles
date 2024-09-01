if &cp || exists('g:loaded_liskin_commands') | finish | endif
let g:loaded_liskin_commands = 1

" {{{1 CloneBuf

function! s:clone_buf()
	let lines = getline(1, '$')
	let ft = &filetype
	new
	silent $put =lines
	let &filetype = ft
endfunction

function! s:clone_buf_tmp()
	call s:clone_buf()

	setlocal buftype=nofile
	setlocal bufhidden=wipe
	setlocal noswapfile
	setlocal nobuflisted
endfunction

command! CloneBuf call s:clone_buf()
command! CloneBufTmp call s:clone_buf_tmp()

" {{{1 DeleteHiddenBuffers

function! s:delete_hidden_buffers()
	let tpbl=[]
	call map(range(1, tabpagenr('$')), {_, t -> extend(tpbl, tabpagebuflist(t))})
	for buf in filter(range(1, bufnr('$')), {_, b -> bufexists(b) && index(tpbl, b)==-1})
		silent execute 'bwipeout' buf
	endfor
endfunction

command! DeleteHiddenBuffers call s:delete_hidden_buffers()

" {{{1 SynInfo, SynStack

function! s:SynInfo()
	echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
		\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
		\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
endfunction

function! s:SynStack()
	echo "vim: " .. string(map(synstack(line('.'), col('.')), {_, s -> synIDattr(s, "name")}))
	if has('nvim')
		Inspect
	endif
endfunction

command! SynInfo call s:SynInfo()
command! SynStack call s:SynStack()

" {{{1 TrimWhitespace

function! s:TrimWhitespace()
	%s/\s*$//
	''
	noh
endfunction

command! TrimWhitespace call s:TrimWhitespace()

" {{{1 GeneratePassword

function! s:apg(mode, len)
	return substitute(system('apg -n 1 -M ' . a:mode . ' -m ' . a:len), '\n\+$', '', '')
endfunction

function! s:xkcdpass(words)
	return substitute(system('xkcdpass -n ' . a:words), '\n\+$', '', '')
endfunction

function! s:GeneratePassword(mode, len = 20)
	exec "normal \"=s:apg(a:mode, a:len)\<Enter>p"
endfunction

function! s:GeneratePasswordXKCD(words = 4)
	exec "normal \"=s:xkcdpass(a:words)\<Enter>p"
endfunction

command! -nargs=? GeneratePassword call s:GeneratePassword("LCN", <f-args>)
command! -nargs=? GeneratePasswordSpecial call s:GeneratePassword("LCNS", <f-args>)
command! -nargs=? GeneratePasswordXKCD call s:GeneratePasswordXKCD(<f-args>)

" {{{1 JoinParagraphs

command! -range=% JoinParagraphs <line1>,<line2>g/^./ .,/^$/-1 join | noh

" {{{1 ConcealToggle

function! s:ConcealOn() abort
	set conceallevel=2 nowrap
endfunction
function! s:ConcealOff() abort
	set conceallevel=0 wrap
endfunction
function! s:ConcealToggle() abort
	if &conceallevel == 0
		call s:ConcealOn()
	else
		call s:ConcealOff()
	endif
endfunction
command! ConcealToggle call s:ConcealToggle()
command! ConcealOn call s:ConcealOn()
command! ConcealOff call s:ConcealOff()

" vim:set foldenable foldmethod=marker: {{{1
