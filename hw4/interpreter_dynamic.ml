open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp
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
  | Procedure (x,e) -> "Procedure "

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

(* represent the given environment as a string (use this for debugging) *)
let rec string_of_env env = 
	(List.fold_left (fun acc (x,v) -> Printf.sprintf "%s, %s |-> %s" acc x (value2str v)) "{" env) ^ "}" 

(* represent the given memory as a string (use this for debugging) *)
let rec string_of_mem mem = 
	(List.fold_left (fun acc (l,v) -> Printf.sprintf "%s, %d |-> %s" acc l (value2str v)) "{" mem) ^ "}" 
		 

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem ->
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_env env x, mem)
  | ADD (e1, e2) -> let (n1, m1) = eval e1 env mem in let (n2, m2) = eval e2 env m1 in (((fun (Int n1) (Int n2) -> Int (n1 + n2)) n1 n2), m2)
  | SUB (e1, e2) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in (n1 - n2 , m2)
  | MUL (e1, e2) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in (n1 * n2 , m2)
  | DIV (e1, e2) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in if n2 = 0 then raise UndefinedSemantics else (n1 / n2, m2)
  | EQ (e1, e2) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in if n1 = n2 then (true, m2) else (false, m2)
  | LT (e1, e2) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in if n1 < n2 then (true, m2) else (false, m2)
  | ISZERO e -> let (n, m1) = eval e in if n = 0 then (true, m1) else (false, m1)
  | READ -> read_int
  | IF (e1, e2, e3) -> let (n1, m1) = eval e1 in let (n2, m2) = eval e2 in let (n3, m3) = eval e3 in if n1 then (n2, m2) else (n3, m3)
  | _ -> raise NotImplemented
	
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
