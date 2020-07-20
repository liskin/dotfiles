if &cp || exists('g:loaded_session')
	finish
endif

let g:loaded_session = 1

let s:auto_save_interval = 5 * 60
let s:last_save = localtime()

function! s:session_filename()
	let l:session_dir = expand("~/.vim/session/")
	if !isdirectory(l:session_dir)
		call mkdir(l:session_dir, "p")
	endif
	return l:session_dir . fnamemodify(".", ":p:gs%[^A-Za-z0-9]%_%")
endfunc

function! s:load_session()
	let l:session = s:session_filename()
	if file_readable(l:session)
		augroup SessionSwapExists
			autocmd!
			autocmd SwapExists * let v:this_session = ""
		augroup END

		execute "source" fnameescape(l:session)
		autocmd! SessionSwapExists

		if !empty(v:this_session)
			let s:last_save = localtime()
		else
			echohl ErrorMsg
			echomsg "Possible session conflict detected, v:this_session unset, use :MkSession to override"
			echohl None
		endif
	endif
endfunc

function! s:save_session()
	if !empty(v:this_session) && !v:dying
		execute "mksession!" fnameescape(v:this_session)
		let s:last_save = localtime()
	endif
endfunc

function! s:auto_load_session()
	" TOOD: allow options that don't open buffers, perhaps? (check argc(), check buffer list, …)
	if len(v:argv) == 1
		call s:load_session()
	endif
endfunc

function! s:auto_save_session()
	if !empty(v:this_session)
		let next_save = s:last_save + s:auto_save_interval
		if localtime() >= next_save
			call s:save_session()
		endif
	endif
endfunc

function! s:new_session()
	let v:this_session = s:session_filename()
	call s:save_session()
endfunc

function! s:rm_session()
	if !empty(v:this_session)
		call delete(v:this_session)
		let v:this_session = ""
	endif
endfunc

command! LoadSession call s:load_session()
command! NewSession call s:new_session()
command! RmSession call s:rm_session()

augroup MySession
	autocmd!
	autocmd VimEnter * nested call s:auto_load_session()
	autocmd VimLeave * call s:save_session()
	autocmd CursorHold * call s:auto_save_session()
augroup END
