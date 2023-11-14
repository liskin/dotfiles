" Vim plugin that resets omnifunc to use ale's completion when it's available

if &cp || exists('g:loaded_ale_reset_omnifunc') | finish | endif
let g:loaded_ale_reset_omnifunc = 1

function! s:on_ready(linter, lsp_details) abort
	let l:id = a:lsp_details.connection_id
	let l:buffer = a:lsp_details.buffer

	if ale#lsp#HasCapability(l:id, 'completion')
		setlocal omnifunc=ale#completion#OmniFunc
	endif
endfunction

function! s:ale_lint_post() abort
	let l:buffer = bufnr('')
	let l:Callback = function('s:on_ready')

	if getbufvar(l:buffer, 'checked_ale_reset_omnifunc', 0)
		return
	else
		call setbufvar(l:buffer, 'checked_ale_reset_omnifunc', 1)
	endif

	for l:linter in ale#linter#Get(getbufvar(l:buffer, '&filetype'))
		if !empty(l:linter.lsp)
			call ale#lsp_linter#StartLSP(l:buffer, l:linter, l:Callback)
		endif
	endfor
endfunction

augroup ALEResetOmnifunc
	autocmd!
	autocmd User ALELSPStarted call s:ale_lint_post()
augroup END
