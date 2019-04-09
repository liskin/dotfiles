if exists('b:did_indent')
    finish
endif

let b:did_indent = 1

setlocal indentexpr=GetHaskellIndent()
setlocal indentkeys=!^F,o,O

function! GetHaskellIndent()
    let line = substitute(getline(getpos('.')[1] - 1), '\t', repeat(' ', &tabstop), 'g')
    let line = substitute(line, '\s*--.*', '', '')

    if line =~ '[!#$%&*+./<=>?@\\^|~\[({-]$\|\<do$'
        return match(line, '\S') + &shiftwidth
    endif

    if line =~ '^\(instance\|class\).*\&.*where$'
        return match(line, '\S') + &shiftwidth
    endif

    if line =~ '[)}\]]$'
	return match(line, '\S')
    endif

    if line !~ '\<else\>'
        let s = match(line, '\<if\>.*\&.*\zs\<then\>')
        if s > 0
            return s
        endif

        let s = match(line, '\<if\>')
        if s > 0
	    return match(line, '\S') + &shiftwidth
        endif
    endif

    let s = match(line, '\<do\s\+\zs[^{]\|\<where\s\+\zs\w\|\<let\s\+\zs\S\|^\s*\zs|\s')
    if s > 0
        return s
    endif

    let s = match(line, '\<case\>\|\<where\>\|\<let\>')
    if s > 0
        return match(line, '\S') + &shiftwidth
    endif

    return match(line, '\S')
endfunction
