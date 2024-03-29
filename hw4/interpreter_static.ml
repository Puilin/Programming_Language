open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
	| MRecProcedure of var * var * exp * var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f
	| MRecProcedure (f,x,e1,g,y,e2,env) -> "MRecProcedure " ^ f ^ " and " ^ g 

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem ->
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_env env x, mem)
  | ADD (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> (Int (n1+n2), m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> (Int (n1-n2), m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | MUL (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> (Int (n1*n2), m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | DIV (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> if n2 = 0 then raise UndefinedSemantics else (Int (n1/n2), m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | EQ (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> if n1 = n2 then (Bool true, m2) else (Bool false, m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | LT (e1, e2) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    (match n1, n2 with
    | Int n1, Int n2 -> if n1 < n2 then (Bool true, m2) else (Bool false, m2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | ISZERO e -> let (n, m1) = eval e env mem in
    (match n with
    | Int n -> if n = 0 then (Bool true, m1) else (Bool false, m1)
    | _ -> raise UndefinedSemantics)
  | READ -> (Int (read_int()), mem)
  | IF (e1, e2, e3) -> let (n1, m1) = eval e1 env mem in
    let (n2, m2) = eval e2 env m1 in
    let (n3, m3) = eval e3 env m1 in
    (match n1 with
    | Bool true -> (n2, m2)
    | Bool false -> (n3, m3)
    | _ -> raise UndefinedSemantics)
  | LET (x, e1, e2) -> let (v1, m1) = eval e1 env mem in
    let row = extend_env (x, v1) env in
    let (v, m2) = eval e2 row m1 in (v, m2)
  | LETREC (f, x, e1, e2) -> let row = extend_env (f, RecProcedure (f,x,e1,env)) env in
    let (v, m1) = eval e2 row mem in (v, m1)
  | LETMREC (f, x, ef, g, y, eg, e3) -> let row = extend_env (f, MRecProcedure (f,x,ef,g,y,eg,env)) env in
    let row = extend_env (g, MRecProcedure (f,x,ef,g,y,eg,env)) row in
    let (v, m1) = eval e3 row mem in (v, m1)
  | PROC (x, e) -> (Procedure (x,e,env), mem)
  | CALL (e1, e2) -> let (a, m1) = eval e1 env mem in
    (match a with
    | Procedure (x,e,rp) -> let (v, m2) = eval e2 env m1 in
      let row = extend_env (x,v) rp in
      let (vprime, m3) = eval e row m2 in (vprime, m3)
    | RecProcedure (f,x,e,rp) -> let (v, m2) = eval e2 env m1 in
      let row = extend_env (x,v) rp in
      let row = extend_env (f, RecProcedure(f,x,e,rp)) row in
      let (vprime, m3) = eval e row m2 in (vprime, m3)
    | _ -> raise UndefinedSemantics)
  | NEWREF e -> let (v, m1) = eval e env mem in
    let l = !counter in
    let mem = extend_mem (l,v) m1 in
    let dummy = new_location() in (Loc l, mem)
  | DEREF e -> let (l, m1) = eval e env mem in
    (match l with
    | Loc l -> let v = apply_mem m1 l in (v, m1)
    | _ -> raise UndefinedSemantics)
  | SETREF (e1, e2) -> let (l, m1) = eval e1 env mem in
    (match l with
    | Loc l -> let (v, m2) = eval e2 env m1 in (v, extend_mem (l,v) m2)
    | _ -> raise UndefinedSemantics)
  | SEQ (e1, e2) -> let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in (v2, m2)
  | BEGIN e -> let (v, m1) = eval e env mem in (v, m1)
  
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
