" Fresh Syntax Vim
" Author : Quinten Palmer


highlight link freshStatement Statement
highlight link freshFunction Function
highlight link freshNumber Number
highlight link freshBool Number

hi def link freshBuiltin freshStatement
hi def link freshTokens freshStatement
hi def link freshExpressionDelimiter freshFunction
hi def link freshDec freshNumber

if exists("b:current_syntax")
	finish
endif

syn keyword freshBuiltin var type function lambda if not and or
syn match freshTokens "[*+><=]"
syn match freshExpressionDelimiter "[()\[\]]"
syn match freshDec "\<\d\+\>"
