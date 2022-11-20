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
    | CONST i            -> [] 
    | VAR   s            -> SUBSET(V(s), C(label))::[]
    | FN    (s1, e2)     -> let temp = gen_equation e2 lst in (SUBSET(T(term), C(label)))::temp 
    | RECFN (s1, s2, e3) -> let temp = gen_equation e3 lst in (SUBSET(T(term), C(label)))::(SUBSET(T(term), V(s1))::temp) 
    | APP   (e1, e2)     -> let temp1 = gen_equation e1 lst in let temp2 = gen_equation e2 lst in let temp = temp1@temp2 in 
                            let cond = app_aux e1 e2 label lst in cond@temp 
    (*TODO*)
    | IF    (e1, e2, e3) -> raise (Failure "undefined") 
    | LET   (s1, e2, e3) -> raise (Failure "undefined") 
    | BOP   (o1, e2, e3) -> raise (Failure "undefined")
and app_aux : exp -> exp -> label -> term list -> constraints
= fun e1 e2 l lst -> 
  let (_, l1) = e1 in let (_, l2) = e2 in match lst with (* materials are prepared *) 
    | hd::tl -> begin match hd with 
        | FN(s1, (_, l0))       -> let temp1 = COND(T(hd), C(l1), C(l2), V(s1)) in
                                   let temp2 = COND(T(hd), C(l1), C(l0), C(l))  in temp1::temp2::(app_aux e1 e2 l tl) 
        | RECFN(s1, _, (_, l0)) -> let temp1 = COND(T(hd), C(l1), C(l2), V(s1)) in 
                                   let temp2 = COND(T(hd), C(l1), C(l0), C(l))  in temp1::temp2::(app_aux e1 e2 l tl)
        | _                     -> raise (Failure "undefined")
        end
    | _      -> [] 
  
let update : 

let solve :

let cfa : exp -> AbsCache.t * AbsEnv.t
=fun exp -> (AbsCache.empty, AbsEnv.empty) (* TODO *)