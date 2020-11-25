let b:undo_ftplugin = b:undo_ftplugin . " | " .
	\ "setl fenc< autoindent< expandtab< listchars<"

let &fenc = &enc

setlocal autoindent
setlocal expandtab

" format=flowed
setlocal fo+=w
setlocal listchars+=trail:\\
