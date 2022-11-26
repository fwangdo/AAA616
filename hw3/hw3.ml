type exp = term * label
and term =
    | CONST of int
    | VAR of string
    | FN of string * exp
    | RECFN of string * string * exp
    | APP of exp * exp
    | IF of exp * exp * exp
    | LET of string * exp * exp
    | BOP of op * exp * exp
and label = int
and op = PLUS | MINUS | MULT | DIV

let string_of_exp (_,l) = string_of_int l
let string_of_term term =
    match term with
    | CONST n -> string_of_int n
    | VAR x -> x
    | FN (x, e) -> "FN " ^ x ^ " -> " ^ string_of_exp e
    | RECFN (f, x, e) -> "RecFN " ^ f ^ " " ^ x ^ " " ^ string_of_exp e
    | APP (e1, e2) -> string_of_exp e1 ^ " " ^ string_of_exp e2
    | IF (e1,e2,e3) -> "IF " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ " " ^ string_of_exp e3
    | LET (x,e1,e2) -> "LET " ^ x ^ " " ^ string_of_exp e1 ^ " " ^ string_of_exp e2
    | BOP (_,e1,e2) -> "BOP " ^ string_of_exp e1 ^ " " ^ string_of_exp e2

let ex1 = (APP ((FN ("x", (VAR "x", 1)) , 2),
                    (FN("y", (VAR "y", 3)), 4)), 5)

let ex2 = (LET ("g", (RECFN("f", "x", ((APP ((VAR "f", 1), 
                                        ((FN ("y", (VAR "y", 2)), 3))), 4))), 5),
                                        (APP((VAR "g", 6), (FN("z", (VAR "z", 7)), 8)), 9)), 10)

let ex3 = (LET ("f", (FN ("x", (VAR "x", 1)), 2),
                            (APP (
                                (APP ((VAR "f", 3), (VAR "f", 4)), 5),
                                (FN ("y", (VAR "y", 6)), 7)), 8)), 9)

type eqn = SUBSET of data * data | COND of data * data * data * data
and data = T of term | C of label | V of string

type constraints = eqn list

module Term = struct
    type t = term
    let compare = compare
end
module Terms = Set.Make(Term)

let string_of_terms terms =
    Terms.fold (fun t s -> s ^ string_of_term t ^ ", ") terms ""

module Label = struct
    type t = label
    let compare = compare
end
module AbsCache = struct
    module Map = Map.Make(Label)
    type t = Terms.t Map.t
    let empty = Map.empty
    let find l m = try Map.find l m with _ -> Terms.empty
    let add l t m = Map.add l (Terms.union t (find l m)) m
    let order m1 m2 = Map.for_all (fun l set -> Terms.subset set (find l m2)) m1
    let print m =
        Map.iter (fun l terms ->
            print_endline (string_of_int l ^ " |-> " ^ string_of_terms terms)
        ) m
    let union_f : label -> Terms.t -> Terms.t -> t -> t 
    = fun x t1 t2 m -> Map.add x (Terms.union t1 t2) m
end

module Var = struct
    type t = string
    let compare = compare
end
module AbsEnv = struct
    module Map = Map.Make(Var)
    type t = Terms.t Map.t
    let empty = Map.empty
    let find l m = try Map.find l m with _ -> Terms.empty
    let add l t m = Map.add l (Terms.union t (find l m)) m
    let order m1 m2 = Map.for_all (fun l set -> Terms.subset set (find l m2)) m1
    let print m =
        Map.iter (fun x terms ->
            print_endline (x ^ " |-> " ^ string_of_terms terms)
        ) m
    let union_f : string -> Terms.t -> Terms.t -> t -> t 
    = fun x t1 t2 m -> Map.add x (Terms.union t1 t2) m
end

(* collecting functions to use in gen_equation.
   specifically, in APP case.*)
let rec collect_func : exp -> term list
= fun exp -> let (term, _) = exp in match term with 
    | FN    _            -> [term] 
    | RECFN _            -> [term] 
    | APP   (e1, e2)     -> (collect_func e1)@(collect_func e2) 
    | IF    (e1, e2, e3) -> (collect_func e1)@(collect_func e2)@(collect_func e3)
    | LET   (_, e2, e3)  -> (collect_func e2)@(collect_func e3)
    | BOP   (_, e2, e3)  -> (collect_func e2)@(collect_func e3)
    | _                  -> []

(* f*)
let rec gen_equation : exp -> term list -> constraints
= fun exp lst -> let term, label = exp in 
    match term with 
    | CONST _            -> [] 
    | VAR   s            -> SUBSET(V(s), C(label))::[]
    | FN    (s1, e2)     -> let temp = gen_equation e2 lst in (SUBSET(T(term), C(label)))::temp 
    | RECFN (s1, s2, e3) -> let temp = gen_equation e3 lst in (SUBSET(T(term), C(label)))::(SUBSET(T(term), V(s1))::temp) 
    | APP   (e1, e2)     -> let temp1 = gen_equation e1 lst in let temp2 = gen_equation e2 lst in let temp = temp1@temp2 in 
                            let cond = app_aux e1 e2 label lst in cond@temp 
    | IF    (e1, e2, e3) -> let temp1 = gen_equation e1 lst in let temp2 = gen_equation e2 lst in let temp3 = gen_equation e3 lst in 
                            let (_, l2) = e2 in let (_, l3) = e3 in let con1 = SUBSET(C(l2), C(label)) in let con2 = SUBSET(C(l3), C(label)) in 
                            con1::con2::(temp1@temp2@temp3)
    | LET   (s1, e2, e3) -> let temp1 = gen_equation e2 lst in let temp2 = gen_equation e3 lst in  
                            let (_, l2) = e2 in let (_, l3) = e3 in let con1 = SUBSET(C(l2), V(s1)) in let con2 = SUBSET(C(l3), C(label)) in 
                            con1::con2::(temp1@temp2)
    | BOP   (o1, e2, e3) -> let temp1 = gen_equation e2 lst in let temp2 = gen_equation e3 lst in temp1@temp2 
and app_aux : exp -> exp -> label -> term list -> constraints
= fun e1 e2 l lst -> 
  let (_, l1) = e1 in let (_, l2) = e2 in match lst with (* materials are prepared *) 
    | hd::tl -> begin match hd with 
        | FN(s1, (_, l0))       -> let temp1 = COND(T(hd), C(l1), C(l2), V(s1)) in
                                   let temp2 = COND(T(hd), C(l1), C(l0), C(l))  in temp1::temp2::(app_aux e1 e2 l tl) 
        | RECFN(_, s2, (_, l0)) -> let temp1 = COND(T(hd), C(l1), C(l2), V(s2)) in 
                                   let temp2 = COND(T(hd), C(l1), C(l0), C(l))  in temp1::temp2::(app_aux e1 e2 l tl)
        | _                     -> raise (Failure "undefined")
        end
    | _      -> [] 
  
(* Check intermediate result. *)
let rec string_of_eqn : eqn -> string 
= fun eqn -> match eqn with 
  | SUBSET (d1, d2)         -> (string_of_data d1) ^ " << " ^ (string_of_data d2) 
  | COND   (d1, d2, d3, d4) -> (string_of_eqn (SUBSET(d1,d2))) ^ " -> " ^ (string_of_eqn (SUBSET(d3, d4))) 
and string_of_data : data -> string 
= fun data -> match data with 
  | T t -> "T " ^ string_of_term t
  | C l -> "C " ^ string_of_int l 
  | V s -> "V " ^ s

let rec string_of_eqns : eqn list -> string option 
= fun lst -> match lst with 
  | hd::tl -> print_endline (string_of_eqn hd); string_of_eqns tl  
  | _ -> None 

let func_list = collect_func ex1
let res = gen_equation ex1 func_list 
(* let _ = string_of_eqns res  *)

(* AbsCache.t * AbsEnv.t are S in pdf *)
(* l would be added in d2' in S *)
let rec update : eqn list -> (AbsCache.t * AbsEnv.t) -> (AbsCache.t * AbsEnv.t)
= fun lst (cache, env) -> match lst with  
  | hd::tl -> begin match hd with 
      | SUBSET (d1, d2) -> if is_label d2 
        then let d2' = get_l d2 in begin match d1 with  
        | T t -> let t'     = Terms.add t Terms.empty in let cache' = AbsCache.add d2' t' cache in update tl (cache', env) 
        | C l -> let d1'    = AbsCache.find l cache in let d2'' = AbsCache.find d2' cache in  
                 let cache' = AbsCache.union_f d2' d1' d2'' cache in update tl (cache', env) 
        | V s -> let d1'    = AbsEnv.find s env in let d2'' = AbsCache.find d2' cache in  
                 let cache' = AbsCache.union_f d2' d1' d2'' cache in update tl (cache', env) 
        end 
        else let d2' = get_v d2 in begin match d1 with 
        | T t -> let t'   = Terms.add t Terms.empty in let env' = AbsEnv.add d2' t' env in update tl (cache, env') 
        | C l -> let d1'  = AbsCache.find l cache in let d2'' = AbsEnv.find d2' env in  
                 let env' = AbsEnv.union_f d2' d1' d2'' env in update tl (cache, env') 
        | V s -> let d1'  = AbsEnv.find s env in let d2'' = AbsEnv.find d2' env in  
                 let env' = AbsEnv.union_f d2' d1' d2'' env in update tl (cache, env') 
        end 
      | COND (d1, d2, d3, d4) -> let d1' = get_term d1 in if is_label d2 
        then let d2' = get_l d2 in let d2_term = AbsCache.find d2' cache in let res = Terms.find_opt d1' d2_term 
             in if (Option.is_none res) then update tl (cache, env) else let next = SUBSET(d3, d4) in update (next::tl) (cache, env) 
        else let d2' = get_v d2 in let d2_term = AbsEnv.find d2' env in let res = Terms.find_opt d1' d2_term 
             in if (Option.is_none res) then update tl (cache, env) else let next = SUBSET(d3, d4) in update (next::tl) (cache, env) 
  end
  | _ -> (cache, env)
and get_term      = function | T x -> x | C _ -> raise (Failure "Impossibe case in update")  | V _ -> raise (Failure "Impossibe case in update")
and get_v         = function | V x -> x | T _ -> raise (Failure "Impossibe case in update")  | C _ -> raise (Failure "Impossibe case in update")
and get_l         = function | C x -> x | T _ -> raise (Failure "Impossibe case in update")  | V _ -> raise (Failure "Impossibe case in update")
and is_label      = function | C _ -> true | V _ -> false | T _ -> raise (Failure "Impossible case in is_label")

let rec solve : eqn list -> (AbsCache.t * AbsEnv.t) -> (AbsCache.t * AbsEnv.t)
= fun lst s -> let (cache, env) = s in  
  let s' = update lst s in let (cache', env') = s' in  
  if (AbsCache.order cache' cache) && (AbsEnv.order env' env) then s 
  else solve lst s'

let cfa : exp -> AbsCache.t * AbsEnv.t
= fun exp -> 
  let func_list = collect_func exp in 
  let eqns = gen_equation exp func_list in 
  solve eqns (AbsCache.empty, AbsEnv.empty) 

let (cache1, env1) = cfa ex1  
let (cache2, env2) = cfa ex2 
let (cache3, env3) = cfa ex3 
let _ = print_endline "\nProblem 1"
let _ = AbsCache.print cache1; AbsEnv.print env1 

let _ = print_endline "\nProblem 2"
let _ = AbsCache.print cache2; AbsEnv.print env2 

let _ = print_endline "\nProblem 3"
let _ = AbsCache.print cache3; AbsEnv.print env3 