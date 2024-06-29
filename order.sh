
ocamlc -c token.ml
ocamlc -c ast.ml
ocamlc -c interpreter.ml
ocamlyacc parser.mly 
ocamlc -c parser.mli
ocamlyacc query.mly
ocamlc -c query.mli 
ocamllex lexer.mll 
ocamllex lexer_query.mll
ocamlc -c lexer.ml 
ocamlc -c lexer_query.ml
ocamlc -c parser.ml 
ocamlc -c query.ml
ocamlc -c main.ml 
ocamlc -o main ast.cmo lexer.cmo lexer_query.cmo parser.cmo query.cmo interpreter.cmo main.cmo 
./main
