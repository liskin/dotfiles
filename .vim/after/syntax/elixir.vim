if !hlexists("elixirId") | finish | endif

syn match elixirKeyword '\<\(do\|else\):'

" sync on top-level and def-level end
syn sync match elixirSync grouphere NONE "^end\s*$"
syn sync match elixirSync grouphere elixirBlock "^  end\s*$"
syn sync minlines=0

hi link elixirBlockDefinition            Keyword
hi link elixirDefine                     Keyword
hi link elixirPrivateDefine              Keyword
hi link elixirGuard                      Keyword
hi link elixirPrivateGuard               Keyword
hi link elixirModuleDefine               Keyword
hi link elixirProtocolDefine             Keyword
hi link elixirImplDefine                 Keyword
hi link elixirRecordDefine               Keyword
hi link elixirPrivateRecordDefine        Keyword
hi link elixirMacroDefine                Keyword
hi link elixirPrivateMacroDefine         Keyword
hi link elixirDelegateDefine             Keyword
hi link elixirOverridableDefine          Keyword
hi link elixirExceptionDefine            Keyword
hi link elixirCallbackDefine             Keyword
hi link elixirStructDefine               Keyword
hi link elixirExUnitMacro                Keyword
hi link elixirKeyword                    Keyword
hi link elixirDocTest                    SpecialComment
