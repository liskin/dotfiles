if exists("b:current_syntax")
	finish
endif

if !exists('main_syntax')
	let main_syntax = 'mail'
endif

runtime! syntax/markdown.vim
unlet! b:current_syntax

if main_syntax ==# 'mail'
	unlet main_syntax
endif
