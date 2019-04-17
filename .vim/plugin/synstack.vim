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
