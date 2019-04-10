if &cp || exists('g:loaded_session')
	finish
endif

let g:loaded_session = 1
let s:saved_session = 0

function! s:save_session()
	if s:saved_session
		mksession! .vimsession
	endif
endfunc

function! s:new_session()
	let s:saved_session = 1
	call s:save_session()
endfunc

command! NewSession call s:new_session()

augroup MySession
	au!
	autocmd VimLeave * call s:save_session()
	autocmd SessionLoadPost * let s:saved_session = 1
augroup END
