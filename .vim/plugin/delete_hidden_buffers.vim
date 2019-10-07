if &cp || exists('g:loaded_delete_hidden_buffers')
	finish
endif

let g:loaded_clonebuf = 1

function! s:delete_hidden_buffers()
	let tpbl=[]
	call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
	for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
		silent execute 'bwipeout' buf
	endfor
endfunction

command! DeleteHiddenBuffers call s:delete_hidden_buffers()
