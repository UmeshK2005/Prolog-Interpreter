open List
open Ast
exception NOT_UNIFIABLE
exception FALSE

                

let a =   [FACT (ATOMIC_FORMULA("fact",[INT(0);INT(1)]));RULE(ATOMIC_FORMULA("fact",[VAR("X");VAR("Y")]),[GE(VAR("X"),INT(0));EQ(VAR("Z"),SUB(VAR("X"),INT(1)));ATOMIC_FORMULA("fact",[VAR("Z");VAR("W")]);EQ(VAR("Y"),MUL(VAR("W"),VAR("X")))])]              

          
let q = GOAL ([ATOMIC_FORMULA("fact",[INT(5);VAR("X")])])
 
let b = [FACT(ATOMIC_FORMULA("parent",[ID("john");ID("mary")]))]
let r = GOAL([ATOMIC_FORMULA("parent",[ID("john");VAR("X")])]) 

let rec vars_terms term = match term with 
    ID(_)->[]
  |INT (_) ->[]
  |ATOMIC_FORMULA (s,lst)->List.fold_left (fun acc x -> acc @ x) [] (List.map vars_terms lst) 
  | VAR (v) ->[v]
  |TUPLE (lst)->List.fold_left (fun acc x -> acc @ x) [] (List.map vars_terms lst)
  |ARRAY (lst)->List.fold_left (fun acc x -> acc @ x) [] (List.map vars_terms lst)
  |GTE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |GE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |LTE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |LE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |EE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |ADD (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |SUB (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |MUL (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |DIV (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |EQ (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
          
let vars_in_atomic_formula (atomic_formula :atomic_formula) : string list = match atomic_formula with 
    ATOMIC_FORMULA (s,lst)->List.fold_left (fun acc x -> acc @ x) [] (List.map vars_terms lst) 
  |INFACT (_)->[]
  |GTE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |GE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |LTE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |LE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |EE (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |ADD (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |SUB (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b 
  |MUL (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |DIV (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
  |EQ (s1,s2) ->
      let a = vars_terms s1  in 
      let b = vars_terms s2 in 
      a@b
          
let vars_in_goal goal  = match goal with 
    GOAL(s)-> List.fold_left (fun acc x -> acc @ x) [] (List.map vars_in_atomic_formula s) 
  


   

let rec searchx x s = match s with 
    []->VAR ""
  |hd::tl ->
      let (s,n) = hd in
      if s = VAR x then 
        n 
      else 
        searchx x tl 
let rec subst s tree = match tree with
    VAR x ->
      let n = searchx x s in 
      if n=VAR "" then 
        VAR x 
      else 
        n 
  |INT(n) -> INT(n)
  |ID s -> ID s
  |ADD(s1,s2)->
      ADD((subst s s1),(subst s s2))
  |MUL(s1,s2)->
      MUL((subst s s1),(subst s s2))
  |DIV(s1,s2)->
      DIV((subst s s1),(subst s s2))
  |SUB(s1,s2)->
      SUB((subst s s1),(subst s s2))
  |ATOMIC_FORMULA(str,lst)->
      ATOMIC_FORMULA(str,map (subst s) lst)
let rec search lst a =match lst with 
    []->false 
  |hd::tl ->
      if hd=a then true 
      else
        search tl a 
let rec searcht x t = match t with 
    VAR y ->
      if x=y then true 
      else 
        false 
  |ATOMIC_FORMULA(str,lst)->
      let l = map (searcht x) lst in 
      let a=search l true in 
      if a= true then true 
      else false 
        
let rec compose_subst s1 s2 = match s1 with
    []->s2 
  |hd::tl ->
      let (s,n) = hd in 
      let t = subst s2 n in 
      let l = compose_subst tl s2 in 
      let rec start_search l s = match l with 
          []->[]
        |hd1::tl1 ->
            let (s3,n3) = hd1 in 
            if s3=s then 
              start_search tl s 
            else 
              let s4 = start_search tl1 s in
              hd1::s4 
      in
      let lt  = start_search l s in
      let p =(s,t) in 
      p::lt 
      
let rec mgu_help tree1 tree2 = match tree1,tree2 with 
    VAR x,VAR y when x=y ->[]
  |VAR x,VAR y->[(VAR x,VAR y)]
  |VAR x, INT n -> [(VAR x, INT n)]
  |INT n, VAR x -> [(VAR x, INT n)]
  |VAR x, ID n -> [(VAR x, ID n)]
  |ID n, VAR x -> [(VAR x, ID n)]
  |INT n1,INT n2 -> 
      if n1 =n2 then []
      else [(VAR "fail",VAR "false")]
  |ID n1,ID n2 -> 
      if n1 =n2 then []
      else [(VAR "fail",VAR "false")]
  |VAR x,ATOMIC_FORMULA(s,lst) ->
      let a = searcht x tree2 in 
      if a=true then 
        [(VAR "fail",VAR "False")]
      else 
        [(VAR x,tree2)]
  |ATOMIC_FORMULA(s,lst),VAR x ->
      let a = searcht x tree1 in 
      if a=true then 
        [(VAR "fail",VAR "False")]
      else 
        [(VAR x,tree1)]
        
  |ATOMIC_FORMULA(s1,lst1),ATOMIC_FORMULA(s2,lst2)->
      if s1<>s2 then 
        [(VAR "fail",VAR "false")]
      else
        let n1 = List.length lst1 in
        let n2 = List.length lst2 in 
        if n1<>n2 then 
          [(VAR "fail",VAR "false")]
        else
          let t1 = List.rev lst1 in
          let t2 = List.rev lst2 in 
          let rec substk k lst1 lst2 = match k with 
              t when t=0 ->[]
            |n when n>0 ->
                let hd1::tl1 = lst1 in 
                let hd2::tl2 = lst2 in
                let k_1 = n-1 in
                let sk_1 = substk k_1 tl1 tl2 in
                let mgusk= mgu_help (subst sk_1 hd1) (subst sk_1 hd2) in 
                let sk = compose_subst sk_1 mgusk in
                sk 
          in 
          let sk = substk n1 t1 t2 in 
          sk 
  |_,_ ->[]
            
            
let mgu tree1 tree2 = 
  let a =mgu_help tree1 tree2 in 
  
  let rec check a = match a with 
      []->true 
    |hd::tl ->
        let (s,n) = hd in 
        if s=VAR "fail" then false 
        else 
          check tl 
  in
  let t = check a in 
  if t= false then raise (NOT_UNIFIABLE)
  else a
    
let rec mgu_lst lst1 lst2 = match lst1,lst2 with 
    [],[]->[]
  |hd1::tl1 ,hd2::tl2->
      try 
        (* print_string "fdfdfdrer\n";*)
        let a= mgu hd1 hd2 in
        
        (* print_string "helloo\n";*)
        let b=mgu_lst tl1 tl2 in
        let c = compose_subst a b in
        c;
      with 
      |NOT_UNIFIABLE ->raise NOT_UNIFIABLE
          
                         
                         
           
        
let rec solve_term (term:term) : term =match term with 
  |ADD(t1,t2) -> 
      let INT(a1) = solve_term t1 in
      let INT(a2) = solve_term t2 in 
      INT(a1+a2)
  |SUB(t1,t2) -> 
      let INT(a1) = solve_term t1 in
      let INT(a2) = solve_term t2 in 
      INT(a1-a2)
  |MUL(t1,t2) -> 
      let INT(a1) = solve_term t1 in
      let INT(a2) = solve_term t2 in 
      INT(a1*a2) 
  |DIV(t1,t2) -> 
      let INT(a1) = solve_term t1 in
      let INT(a2) = solve_term t2 in 
      INT(a1/a2)
  |_ ->term 

             
let rec eval (atomic_formula :atomic_formula) mgu_list  = match atomic_formula with 
  
    EQ(s1,s2)-> 
      
      let a1 = subst mgu_list s1 in
      let a2 = subst mgu_list s2 in
      (* print_string "hlhl\n";*)
      let a3 = solve_term a1 in 
      let a4 = solve_term a2 in 
      
      compose_subst mgu_list (mgu a3 a4)
  | GTE(s1,s2) ->
      let a1 = subst mgu_list s1 in
      let a2 = subst mgu_list s2 in
      let INT(a3) = solve_term a1 in 
      let INT(a4) = solve_term a2 in 
      if a3>=a4 then mgu_list else raise NOT_UNIFIABLE
  | GE(s1,s2) ->
      let a1 = subst mgu_list s1 in
      let a2 = subst mgu_list s2 in
      let INT(a3) = solve_term a1 in 
      let INT(a4) = solve_term a2 in 
      if a3>a4 then mgu_list else raise NOT_UNIFIABLE
 
          
let rec match_goal atomic_formula clause_list = match clause_list with 
    []->raise (NOT_UNIFIABLE) 
  |hd::tl ->
      match hd with 
        FACT(ATOMIC_FORMULA(s,lst))->
          let ATOMIC_FORMULA(s1,lst1) = atomic_formula in
          if s=s1 then 
            mgu (ATOMIC_FORMULA(s,lst)) (ATOMIC_FORMULA(s1,lst1))
          else 
            match_goal atomic_formula tl 
          
let rec find_match (atomic_formula :atomic_formula) clause_list unif_list= match clause_list with 
    []-> ([(VAR "fail",VAR "false")],[])
  |hd::tl->
      match hd with 
        FACT(ATOMIC_FORMULA(s1,lst1))->
          begin
            (*print_string "rathi\n";*)
            let ATOMIC_FORMULA(s2,lst2) = atomic_formula in
            if s1<>s2 then find_match atomic_formula tl unif_list
            else 
              let l1 = List.length lst1 in
              let l2 = List.length lst2 in
              if l1<>l2 then find_match atomic_formula tl unif_list
              else 
                try 
                  (* print_string "sahu\n";*)
                  let l1 = List.map (subst unif_list) lst1 in
                  let l2 = List.map (subst unif_list) lst2 in
                  (* print_string "sahu\n";*)
                  let a =mgu_lst l1 l2 in
                  
                  (*print_string "adadasa\n";*)
                  let b= compose_subst unif_list a in
                  (b,[]) ;
                with 
                  NOT_UNIFIABLE -> find_match atomic_formula tl unif_list
          end
      |RULE(ATOMIC_FORMULA(s1,lst1),lt)->
          begin
            (* print_string "ssdsd\n";*)
            let ATOMIC_FORMULA(s2,lst2) = atomic_formula in
            if s1<>s2 then find_match atomic_formula tl unif_list
            else 
              let l1 = List.length lst1 in
              let l2 = List.length lst2 in
              if l1<>l2 then find_match atomic_formula tl unif_list
              else 
                try
                  let l1 = List.map (subst unif_list) lst1 in
                  let l2 = List.map (subst unif_list) lst2 in
                  let a =mgu_lst l1 l2 in
                  
                  let b= compose_subst unif_list a in
                  (b,lt) ;
                with 
                  NOT_UNIFIABLE ->find_match atomic_formula tl unif_list
          end
let rec new_term term = match term with 
    VAR x -> 
      let s = "1" ^x in
      VAR (s)
  |INT n ->INT n
  |ID s ->ID s
  |GTE (s1,s2)->
      let p1 = new_term s1 in
      let p2 = new_term s2 in 
      GTE(p1,p2)
  |SUB (s1,s2)->
      let p1 = new_term s1 in
      let p2 = new_term s2 in 
      SUB(p1,p2)
  |MUL (s1,s2)->
      let p1 = new_term s1 in
      let p2 = new_term s2 in 
      MUL(p1,p2)
  |ATOMIC_FORMULA(s,lst)->
      let l = new_term_list lst in 
      ATOMIC_FORMULA(s,l)
and new_term_list term_list = match term_list with 
    []->[]
  |hd::tl ->
              (*print_string "hi\n";*)
      let p =new_term hd in 
              (* print_string "hiii\n";*)
      let q = new_term_list tl in 
      p::q
      
let rec new_atomic_formula (atomic_formula : atomic_formula) :atomic_formula = match atomic_formula with 
    ATOMIC_FORMULA(s,lst)->
      let a = new_term_list lst in
      ATOMIC_FORMULA(s,a)
  |GTE(s1,s2)->
      let a = new_term s1 in 
      let b= new_term s2 in 
      GTE(a,b) 
  |EQ(s1,s2)->
      let a = new_term s1 in 
      let b= new_term s2 in 
      EQ(a,b)
  |GE(s1,s2)->
      let a = new_term s1 in 
      let b= new_term s2 in 
      GE(a,b)
        
let rec new_atomic_formula_list (atomic_formula_list :body) :body = match atomic_formula_list with 
    []->[]
  |hd::tl ->
      let a = new_atomic_formula hd in
      let b = new_atomic_formula_list tl in
      a::b 
      
let rec new_prog prog = match prog with 
    FACT(ATOMIC_FORMULA(a,lst))->
              (* print_string "enter\n";*)
      let p = new_term_list lst in 
      FACT(ATOMIC_FORMULA(a,p))
  |RULE(a,lt)->
      let ATOMIC_FORMULA(s,lst) = a in
      let p =new_term_list lst in 
              (* print_string "fdfdfdfcx\n";*)
      let q = new_atomic_formula_list lt in
      RULE(ATOMIC_FORMULA(s,p),q)
        
let rec new_prog_list (prog_list) = match prog_list with 
    []->[]
  |hd::tl->
              (*   print_string "fdfd\n";*)
      let p = new_prog hd in
              (*  print_string "fdfd1";*)
      let q = new_prog_list tl in
              
      p::q 
      
let rec solve program goal mgu_list varlst = match goal with 
    GOAL([])->(true , mgu_list) 
  |GOAL(hd::tl)-> match hd with 
    |GTE(s1,s2)
    |GE (s1,s2)
    |LTE (s1,s2)
    |LE (s1,s2)
    |EQ (s1,s2)
    |EE (s1,s2) -> (
        (* print_int (List.length tl);
         print_string "dsd\n";*)
        try solve program (GOAL(tl)) (eval hd mgu_list) varlst
        with NOT_UNIFIABLE -> (false, [])
      )
      
    |INFACT(_) -> let a = (solve program (GOAL(tl)) mgu_list varlst) in 
        (true, [])
    |_ ->
        (* print_string "f";*)
        
        let new_program = new_prog_list program in
        (* print_string "hiii\n";*)
        let (unif,alst) = find_match hd program mgu_list in
        (* print_int (List.length unif);
          print_string "ffdfdfdfdfdfdfdfdfd\n";*)
        
        let rec check a = match a with 
            []->true 
          |hd::tl ->
              let (s,n) = hd in 
              if s=VAR "fail" then false 
              else 
                check tl 
        in
        let t = check unif in 
        if t=false then (false, [])
        else 
          
          let u = solve new_program (GOAL(alst@tl)) unif varlst in
          u 
 
            
let rec search_ans x u =match u with 
    []->INT 0
  |hd::tl->
      let (VAR a,b)= hd in
      if a=x then b
      else 
        search_ans x tl
 
          

let real_solve program goal = 
  let new_program = new_prog_list program in
  let u = solve new_program goal [] [] in
  let lt = vars_in_goal goal in 
  let rec search_var lt u = match lt with 
      []->[]
    |hd::tl->
        let p=search_ans hd u in 
        let q=search_var tl u in
        (VAR hd,p)::q 
  in
  let (p,q)=u in 
  if p=true then 
    search_var lt q
  else
    raise FALSE
        







