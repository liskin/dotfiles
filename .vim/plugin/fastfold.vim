" https://vim.fandom.com/wiki/Keep_folds_closed_while_inserting_text
"
" Don't screw up folds when inserting text that might affect them, until
" leaving insert mode. Foldmethod is local to the window. Protect against
" screwing up folding when switching between windows.
"
" Also, perhaps more importantly, avoid the performance penalty of recomputing
" fold after every inserted character.

if &cp || exists('g:loaded_fastfold') | finish | endif
let g:loaded_fastfold = 1

function! s:fold_save() abort
	if !exists('w:last_fdm')
		let w:last_fdm=&l:foldmethod
		setlocal foldmethod=manual
	endif
endfunction

function! s:fold_restore() abort
	if exists('w:last_fdm')
		let &l:foldmethod=w:last_fdm
		unlet w:last_fdm
	endif
endfunction

augroup FastFold
	autocmd!
	autocmd InsertEnter * call s:fold_save()
	autocmd InsertLeave,WinLeave * call s:fold_restore()
augroup END
