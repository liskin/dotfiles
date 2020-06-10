if exists("b:did_ftplugin_taskwiki_after") | finish | endif
let b:did_ftplugin_taskwiki_after = 1

function! s:follow_link(split)
	let syn_stack = map(synstack(line('.'), col('.')), {_, s -> synIDattr(s, "name")})
	let inside_link = index(syn_stack, 'VimwikiLink') != -1
	if inside_link
		execute a:split ? "VimwikiSplitLink" : "VimwikiFollowLink"
	else
		execute "py3 Mappings.task_info_or_vimwiki_follow_link(split=" . (a:split ? "True" : "False") . ")"
	endif
endfunction

nnoremap <silent><buffer> <CR> :call <SID>follow_link(0)<CR>
nnoremap <silent><buffer> <C-W><CR> :call <SID>follow_link(1)<CR>
nnoremap <silent><buffer> <C-]> :call <SID>follow_link(0)<CR>
nnoremap <silent><buffer> <C-W><C-]> :call <SID>follow_link(1)<CR>

function! s:refresh() abort
	let data_location = py3eval("cache().get_relevant_tw().config.get('data.location')")
	let task_data_mod = map(glob(data_location . "/*.data", 1, 1), {_, f -> getftime(f)})
	let file_mod = getftime(expand("%"))
	let older_than_task_data = !empty(filter(task_data_mod, {_, data_mod -> data_mod > file_mod}))
	let older_than_today = strptime("%F", strftime("%F")) > file_mod
	if older_than_task_data || older_than_today
		TaskWikiBufferLoad
		write
	endif
endfunction

function! s:refresh_if_safe() abort
	if mode(1) ==# 'n' && !&modified
		call s:refresh()
	endif
endfunction

augroup taskwikiRefresh
	autocmd! * <buffer>
	autocmd FocusGained <buffer> call s:refresh_if_safe()
	autocmd BufEnter <buffer> call timer_start(0, {-> s:refresh_if_safe()})
augroup END

call timer_start(0, {-> s:refresh()})
