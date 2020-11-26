let b:undo_ftplugin = b:undo_ftplugin . " | " .
	\ "setl fenc< autoindent< expandtab< listchars<"

let &fenc = &enc

setlocal autoindent
setlocal expandtab

" format=flowed
setlocal formatoptions+=w
setlocal listchars+=trail:\\

" markdown
setlocal formatoptions+=n
setlocal comments=fb:*,fb:-,fb:+,n:>
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
