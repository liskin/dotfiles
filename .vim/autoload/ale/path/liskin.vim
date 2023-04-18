" Given a buffer and a filename, find the furthest file by searching upwards
" through the paths relative to the given buffer.
function! ale#path#liskin#FindFurthestFile(buffer, filename) abort
	let l:best_path = ''

	let l:last_dir = ''
	let l:current_dir = fnamemodify(bufname(a:buffer), ':p:h')
	while l:last_dir !=# l:current_dir
		let l:relative_path = findfile(a:filename, fnameescape(l:current_dir) . ';')
		if !empty(l:relative_path)
			let l:best_path = fnamemodify(l:relative_path, ':p')

			let l:last_dir = l:current_dir
			let l:current_dir = fnamemodify(l:best_path, ':h:h')
		else
			return l:best_path
		endif
	endwhile

	return l:best_path
endfunction
