" simpler (way faster) indent for Elixir

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

setlocal indentexpr=GetElixirIndent()
setlocal indentkeys=!^F,o,O,<CR>,0=end,0=after,0=catch,0=else,0=rescue

if exists("*GetElixirIndent")
	finish
endif

function! s:GetLine(lnum)
	let line = substitute(getline(a:lnum), '\s*# .*', '', '')
	return line
endfunction

function! s:HeredocAtEnd(lnum)
	let s = synIDattr(synID(a:lnum, col([a:lnum, '$']) - 1, 1), "name")
	return s =~# '^elixir\%(Sigil\|String\|DocString\)$'
endfunction

function! GetElixirIndent()
	" Find a non-blank line above the current line.
	let prevlnum = prevnonblank(v:lnum - 1)

	" Hit the start of the file, use zero indent.
	if prevlnum == 0
		return 0
	endif

	let ind = indent(prevlnum)
	let prevline = s:GetLine(prevlnum)
	let curlnum = v:lnum
	let curline = s:GetLine(curlnum)

	if s:HeredocAtEnd(prevlnum)
		return ind
	endif

	if prevline =~ '\%(^\|\s\)\%(do\|after\|catch\|else\|rescue\|->\)$'
		let ind += &shiftwidth
	endif

	if curline =~ '^\s*\%(end\|after\|catch\|else\|rescue\)\>'
		let ind -= &shiftwidth
	endif

	return ind
endfunction
