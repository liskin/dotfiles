" Vim plugin to make bracketed paste fast by temporarily disabling foldexpr

if &cp || exists('g:loaded_fastpaste')
	finish
endif

let g:loaded_fastpaste = 1

function! s:paste_toggled(new, old) abort
	if a:new && !a:old
		let b:saved_foldexpr = &foldexpr
		let &l:foldexpr = ''
	elseif !a:new && a:old && exists('b:saved_foldexpr')
		let &l:foldexpr = b:saved_foldexpr
		unlet b:saved_foldexpr
	endif
endfunc

augroup FastPaste
	autocmd!
	autocmd OptionSet paste call s:paste_toggled(v:option_new, v:option_old)
augroup END
