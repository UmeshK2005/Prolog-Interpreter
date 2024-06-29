

type ast= clause list
and clause = FACT of atomic_formula | RULE of atomic_formula * body
and body = atomic_formula list
and atomic_formula = ATOMIC_FORMULA of string * term list|INFACT of string|GTE of term * term |GE of term * term |LTE of term * term |LE of term * term |EE of term * term |ADD of term * term |SUB of term * term |MUL of term * term |DIV of term * term|ASSIGN of term list * term list |EQ of term*term
and term = ID of string |INT of int |ATOMIC_FORMULA of string * term list| VAR of string|TUPLE of term list|ARRAY of term list|GTE of term * term |GE of term * term |LTE of term * term |LE of term * term |EE of term * term |ADD of term * term |SUB of term * term |MUL of term * term |DIV of term * term|EQ of term*term
type goal = GOAL of atomic_formula list
