syn match elixirKeyword '\<\(do\|else\):'

" sync on end
syn sync match elixirSync grouphere elixirBlock "^\s*end\>"
" but at least 100 lines to reduce likelihood of being confused by a lone end in a heredoc
syn sync minlines=100

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
