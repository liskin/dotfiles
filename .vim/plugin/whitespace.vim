if &cp || exists('g:loaded_whitespace')
	finish
endif

let g:loaded_whitespace = 1

function! s:TrimWhitespace()
	%s/\s*$//
	''
	noh
endfunction

command! TrimWhitespace call s:TrimWhitespace()
