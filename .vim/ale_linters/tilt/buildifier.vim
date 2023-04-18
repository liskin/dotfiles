runtime autoload/ale/fixers/buildifier.vim

function! ale_linters#tilt#buildifier#GetProjectRoot(buffer) abort
	return ale#path#Dirname(
		\ ale#path#liskin#FindFurthestFile(a:buffer, 'Tiltfile')
	\ )
endfunction

function! ale_linters#tilt#buildifier#GetCommand(buffer) abort
	let l:options = ale#Var(a:buffer, 'bazel_buildifier_options')
	return '%e' . ale#Pad(l:options) . ' -mode check -lint warn -format json -path %s -'
endfunction

function! ale_linters#tilt#buildifier#Handle(buffer, lines) abort
	try
		let l:json = json_decode(join(a:lines, ''))
	catch
		return []
	endtry

	let l:output = []
	let l:dir = expand('#' . a:buffer . ':p:h')

	for l:file in l:json['files']
		let l:abs_file = ale#path#GetAbsPath(l:dir, l:file['filename'])
		for l:warning in l:file['warnings']
			call add(l:output, #{
				\ filename: l:abs_file,
				\ lnum: l:warning['start']['line'],
				\ col: l:warning['start']['column'],
				\ end_lnum: l:warning['end']['line'],
				\ end_col: l:warning['end']['column'],
				\ type: 'W',
				\ code: l:warning['category'],
				\ text: l:warning['message'],
			\ })
		endfor
	endfor

	return l:output
endfunction

call ale#linter#Define('tilt', #{
	\ name: 'buildifier',
	\ executable: {b -> ale#Var(b, 'bazel_buildifier_executable')},
	\ command: function('ale_linters#tilt#buildifier#GetCommand'),
	\ cwd: function('ale_linters#tilt#buildifier#GetProjectRoot'),
	\ callback: function('ale_linters#tilt#buildifier#Handle'),
\ })
