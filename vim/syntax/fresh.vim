" Fresh Syntax Vim
" Author : Quinten Palmer

if exists("b:current_syntax")
	finish
endif

" Numbers
syn match freshDec "\<\d\+\>"

hi def link freshDec Number

" Booleans
syn keyword freshBool true false

hi def link freshBool Number

" Builtins and Statements
syn keyword freshBuiltin var function lambda if not and or member
syn match freshTokens "[*+><=]"

hi def link freshBuiltin Statement
hi def link freshTokens Statement

" Expressions
syn match freshExpressionDelimiter "[()\[\]]"

hi def link freshExpressionDelimiter Function

" Name of functions and variables
syn match freshName "\%(\%(var\s\|function\s\)\s*\)\@<=\h\w*"

hi def link freshName Function

" Packages and Imports
syn keyword freshPackage package
syn keyword freshInclude import

hi def link freshPackage Include
hi def link freshInclude Include

" Types
syn keyword freshType type struct

hi def link freshType Type
