if exists("b:did_ftplugin_vimwiki_liskin_after") | finish | endif
if !exists(":VimwikiFollowLink") || !exists(":TaskWikiBufferLoad") | finish | endif
let b:did_ftplugin_vimwiki_liskin_after = 1

setlocal et
let &l:wrap = fnamemodify(expand('%'), ':p:.') =~# '^notes/'

command! -buffer -range TaskWikiNote :<line1>,<line2>call vimwiki#liskin#TaskWikiNote()
nnoremap <silent><buffer> <LocalLeader>n :TaskWikiNote<CR>

command! -buffer -range TaskWikiOpen :<line1>,<line2>call vimwiki#liskin#TaskWikiOpen()
nnoremap <silent><buffer> <LocalLeader>o :TaskWikiOpen<CR>

nnoremap <silent><buffer> <CR> :call vimwiki#liskin#follow_link()<CR>
nnoremap <silent><buffer> <C-]> :call vimwiki#liskin#follow_link()<CR>
nmap <silent><buffer> <C-W><CR> <Plug>VimwikiSplitLink
nmap <silent><buffer> <C-W><C-]> <Plug>VimwikiSplitLink

augroup taskwikiRefresh
	autocmd! * <buffer>
	autocmd FocusGained <buffer> call vimwiki#liskin#refresh_if_safe()
	autocmd BufEnter <buffer> call timer_start(0, {-> vimwiki#liskin#refresh_if_safe()})
augroup END

call timer_start(0, {-> vimwiki#liskin#refresh()})
