if &cp || exists('g:loaded_clonebuf')
	finish
endif

let g:loaded_clonebuf = 1

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
