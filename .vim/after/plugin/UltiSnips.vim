if &cp || exists('g:loaded_after_ultisnips') | finish | endif
let g:loaded_after_ultisnips = 1

" disable UltiSnips_AutoTrigger after every keystroke
augroup UltiSnips_AutoTrigger
	autocmd!
augroup END
