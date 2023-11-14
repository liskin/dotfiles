let g:float_preview#docked = 0
autocmd User FloatPreviewWinOpen call s:FloatPreviewStyle()
function! s:FloatPreviewStyle() abort
	call nvim_win_set_config(g:float_preview#win, #{border: 'rounded'})
endfunction
