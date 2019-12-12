if &cp || exists('g:loaded_liskin_commands')
	finish
endif

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
	call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
	for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
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
	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
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

" vim:set foldenable foldmethod=marker: {{{1
