call ale#Set('tilt_lsp_config', {})

function! ale_linters#tilt#tilt_lsp#GetProjectRoot(buffer) abort
	return ale#path#Dirname(
		\ ale#path#liskin#FindFurthestFile(a:buffer, 'Tiltfile')
	\ )
endfunction

call ale#linter#Define('tilt', #{
	\ name: 'tilt_lsp',
	\ lsp: 'stdio',
	\ command: 'tilt lsp start',
	\ executable: 'tilt',
	\ project_root: function('ale_linters#tilt#tilt_lsp#GetProjectRoot'),
	\ lsp_config: {b -> ale#Var(b, 'tilt_lsp_config')},
\ })
