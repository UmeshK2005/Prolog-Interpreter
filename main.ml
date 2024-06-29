open Lexing
open Printf
open Parser
open Ast
open Interpreter
open Query

let rec print_ast ast =

  match ast with

  | [] -> printf "EMPTY\n"

  | clause_list ->

      printf "AST(\n";

      print_char '\t';

      let a =List.iter print_clause clause_list in

      print_string ")\n"


and print_clause clause =

  match clause with

  | FACT atomic_formula ->

      printf "FACT(\n";

      print_string "\t \t";

      print_atomic_formula atomic_formula;

      printf ")\n"

  | RULE (atomic_formula, body) ->

      printf "RULE(\n";

      print_string "\t \t";

      print_atomic_formula atomic_formula;

      printf " , \n";

      print_string "\t \t";

      print_body body;

      printf "\n \t )\n"


and print_body body =

  match body with

  | [] -> ()

  | [atomic_formula] -> 

      print_atomic_formula atomic_formula

  | atomic_formula :: rest ->

      print_atomic_formula atomic_formula;

      printf ", \n";

      print_body rest


and print_atomic_formula atomic_formula =

  match atomic_formula with

  | ATOMIC_FORMULA (name, terms) ->

      print_string "Atomic_formula (\n";

      printf "\t \t \t ID %s(" name;

      print_terms terms;

      printf "))\n \t \t \t "

      

  | INFACT name -> printf "FACT( %s" name

  | ASSIGN (name1,name2) -> 

  	print_string "ASSIGN (\n \t \t \t ";

  	print_terms name1 ;

  	print_string " , ";

  	print_string "\n \t \t \t ";

  	print_terms name2 ;

  	print_string ")\n"

  | GTE (name1,name2) -> 

     print_string "GTE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | GE (name1,name2) -> 

     print_string "GE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | LTE (name1,name2) -> 

     print_string "LTE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | EE (name1,name2) -> 

     print_string "EE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | LE (name1,name2) -> 

     print_string "LE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | ADD (name1,name2) -> 

     print_string "ADD (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | MUL (name1,name2) -> 

     print_string "MUL (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | DIV (name1,name2) -> 

     print_string "DIV (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | SUB (name1,name2) -> 

     print_string "SUB (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"



and print_terms terms =

  match terms with

  | [] -> ()

  | [term] -> print_term term

  | term :: rest ->

     print_term term;

      

      printf ", ";

      print_terms rest


and print_term term =

  match term with

  | ID s -> printf "%s ID" s

  | INT n -> printf "%d INT" n

  | ATOMIC_FORMULA (name, p) -> printf "%s ATOMIC_FORMULA (" name;

  let a =print_terms p in 

  print_string ")"

  |VAR s -> printf "%s VAR" s

  |TUPLE (name) -> 

  	print_string "TUPLE ( ";

  	let a= print_terms name in

	print_string ")"

  |ARRAY (name) -> 

  	print_string "ARRAY ( ";

  	let a= print_terms name in

	print_string ")"
  | GTE (name1,name2) -> 

     print_string "GTE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | GE (name1,name2) -> 

     print_string "GE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | LTE (name1,name2) -> 

     print_string "LTE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | EE (name1,name2) -> 

     print_string "EE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | LE (name1,name2) -> 

     print_string "LE (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | ADD (name1,name2) -> 

     print_string "ADD (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | MUL (name1,name2) -> 

     print_string "MUL (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | DIV (name1,name2) -> 

     print_string "DIV (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"

  | SUB (name1,name2) -> 

     print_string "SUB (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"
  | EQ (name1,name2) -> 

     print_string "EQ (\n \t \t \t ";

     print_term name1;

     print_string " , ";

     print_string "\n \t \t \t ";

     print_term name2;

     print_string ")\n"


let rec string_of_term (term :term) :string =
  match term with
  | ID s -> s
  | INT i -> string_of_int i
  | VAR v -> v
  

let print_term_pair_list pairs =
  let string_of_pair (t1, t2) = "(" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ")" in
  let pairs_str = List.map string_of_pair pairs |> String.concat "; " in
  print_endline ("[" ^ pairs_str ^ "]")




let read_file (filename : string) : string =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let parse_input lexbuf =
  try
    Parser.program Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      eprintf "Syntax error at line %d, position %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1
      
      
let parse_query lexbuf =
  try
    Query.goal Lexer_query.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      eprintf "Syntax error at line %d, position %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1



let _=
      
  let filename = "program.txt" in
  let prolog_code = read_file filename in
  let lexbuf = Lexing.from_string prolog_code in
  
  let query = Lexing.from_channel stdin in
  
  let ast = parse_input lexbuf in
  let query_ast = parse_query query in
  let d = Interpreter.real_solve ast query_ast in
  print_term_pair_list d;
  

