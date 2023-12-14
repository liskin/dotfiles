" Vim plugin that resolves symlinks of opened files and makes filenames relative

if &cp || exists('g:loaded_canon_filename') | finish | endif
let g:loaded_canon_filename = 1

function! s:canon_filename(new_file) abort
	let f = simplify(expand("%"))
	let f_canon = resolve(f)

	" try to detect if the buffer is in any way special: plugins that rely on
	" BufWriteCmd should set buftype=acwrite, but not all do, so test the
	" other options to mark the buffer special
	if empty(f) || !&write || !empty(&buftype) || !empty(&bufhidden) || !&swapfile || !&buflisted || (!filereadable(f) && !a:new_file) || f ==# f_canon
		return
	endif

	" set filename (forces BF_NOTEDITED and prevents further writes)
	execute "silent" "keepalt" "file" fnameescape(f_canon)

	" reset BF_NOTEDITED
	set buftype=acwrite
	augroup CanonFilenameHelper
		autocmd! BufWriteCmd <buffer>
		autocmd BufWriteCmd <buffer> echo
		" this may still trigger other BufWriteCmds but the special buffer
		" detection makes this very unlikely
		w!
		autocmd! BufWriteCmd <buffer>
	augroup END
	set buftype=
endfunc

augroup CanonFilename
	autocmd!
	autocmd BufRead * ++nested call s:canon_filename(v:false)
	autocmd BufNewFile * ++nested call s:canon_filename(v:true)
augroup END
