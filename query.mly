%{
 
open Ast
%}
%token <int> INT 
%token <string> ID 
%token <string> VAR
%token  ADD MUL DIV SUB 
%token  PIPE
%token  GE LE EE EQ
%token  IF GTE LTE IS
%token   CUT END COMMA
%token  LPAREN RPAREN LBRAC RBRAC
%token  EOF

%start goal
%type <Ast.goal> goal 
  
%% 

goal :
	atomic_formula_list END {GOAL($1)}
atomic_formula_list :
	atomic_formula {[$1]}
	|atomic_formula COMMA atomic_formula_list {$1::$3}	

	
	
atomic_formula :
	ID LPAREN term_list RPAREN {
	(*print_string "4\n";*)
	ATOMIC_FORMULA ($1,$3)}
	|term GTE term {GTE($1,$3)}
	|term GE term {GE($1,$3)}
	|term LTE term {LTE($1,$3)}
	|term LE term {LE($1,$3)}
	|term EE term {EE($1,$3)}
	|term EQ term {EQ($1,$3)}
	|term SUB term {SUB($1,$3)}
	|term DIV term {DIV($1,$3)}
	|term MUL term {MUL($1,$3)}
	|term ADD term {ADD($1,$3)}
	|term_list IS term_list {(*print_string "12\n";*)ASSIGN($1,$3)}
	|CUT {
	(*print_string "6\n";*)
	INFACT("!")}

term_list:
	term {
	(*print_string "7\n";*)
	[$1]}
	| term COMMA term_list {
	(*print_string "8\n";*)
	$1 :: $3}
	| term PIPE term_list {
	$1 :: $3}
	
	
	
term:
	ID {
	(*print_string "11\n";*)
	ID $1}
	| INT {INT $1}
	| VAR {
	(*print_string "9\n";*)
	VAR $1}
	|LBRAC term_list RBRAC {ARRAY($2)}
	|LPAREN term_list RPAREN{
	(*print_string "10\n";*)
	TUPLE($2)}
	
	|ID LPAREN term_list RPAREN {ATOMIC_FORMULA($1,$3)}
	|term GTE term {GTE($1,$3)}
	|term GE term {GE($1,$3)}
	|term LTE term {LTE($1,$3)}
	|term LE term {LE($1,$3)}
	|term EE term {EE($1,$3)}
	|term EQ term {EQ($1,$3)}
	|term SUB term {SUB($1,$3)}
	|term DIV term {DIV($1,$3)}
	|term MUL term {MUL($1,$3)}
	|term ADD term {ADD($1,$3)}
	
%%
	
	
	
	
	
	
	
	
	
	
	
	
