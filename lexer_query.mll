
{open Query}

let letter =['a'-'z' 'A'-'Z']
let s_letter = ['a'-'z']
let st_letter =['_' 'A'-'Z']
let digit = ['0'-'9']
let underscore = '_'
rule token =parse 
	[' ' '\t' '\n' ]{token lexbuf}
	| "is" {IS}
	| ['+'] {ADD}
	| ['*'] {MUL}
	| ['-'] {SUB}
	| ['/'] {DIV}
	| "<=" {LTE}
	| "==" {EE}
	| ['<'] {LE}
	| ['>'] {GE}
	| ">=" {GTE}
	| s_letter(letter|digit|underscore)* as id {ID id}
	| st_letter(letter|digit|underscore)* as id {VAR id}
	| [','] {COMMA}
	| ['|'] {PIPE}
	| ['.'] {END}
	| ['('] {LPAREN}
	| [')'] {RPAREN}
	| ['['] {LBRAC}
	| [']'] {RBRAC}
	| ":-" {IF}
	| ['!'] {CUT}
	| ['='] {EQ}
	| ['0'-'9']+ as num {INT(int_of_string num)}
	|eof {EOF}
