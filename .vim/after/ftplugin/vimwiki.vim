if exists("b:did_ftplugin_taskwiki_after") | finish | endif
let b:did_ftplugin_taskwiki_after = 1

nnoremap <silent><buffer> <CR> :call vimwiki#liskin#follow_link(0)<CR>
nnoremap <silent><buffer> <C-W><CR> :call vimwiki#liskin#follow_link(1)<CR>
nnoremap <silent><buffer> <C-]> :call vimwiki#liskin#follow_link(0)<CR>
nnoremap <silent><buffer> <C-W><C-]> :call vimwiki#liskin#follow_link(1)<CR>

augroup taskwikiRefresh
	autocmd! * <buffer>
	autocmd FocusGained <buffer> call vimwiki#liskin#refresh_if_safe()
	autocmd BufEnter <buffer> call timer_start(0, {-> vimwiki#liskin#refresh_if_safe()})
augroup END

call timer_start(0, {-> vimwiki#liskin#refresh()})
