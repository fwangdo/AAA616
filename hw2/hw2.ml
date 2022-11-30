type lv = 
  | Var of string
  | Ptr of lv 

type exp = 
  | Const of int
  | Plus of exp * exp
  | Mult of exp * exp
  | Sub of  exp * exp
  | Lv of lv   (*  l-value *)
  | Loc of lv  (* &l-value *)

type bexp = 
  | True 
  | False
  | Equal of exp * exp
  | Le of exp * exp
  | Not of bexp
  | And of bexp * bexp

type cmd = 
  | Assign of lv * exp
  | Alloc of lv 
  | Seq of cmd list
  | If of bexp * cmd * cmd
  | While of bexp * cmd
  | Skip

let rec string_of_lv : lv -> string 
= fun l -> match l with
  | Var s -> "Var " ^ s 
  | Ptr s -> "Ptr " ^ (string_of_lv s) 

let rec string_of_exp : exp -> string 
= fun a -> match a with
  | Const n -> string_of_int n
  | Plus (a1, a2) -> string_of_exp a1 ^ " + " ^ string_of_exp a2
  | Mult (a1, a2) -> string_of_exp a1 ^ " * " ^ string_of_exp a2
  | Sub  (a1, a2) -> string_of_exp a1 ^ " - " ^ string_of_exp a2
  | Lv   lv       -> string_of_lv lv
  | Loc  lv       -> string_of_lv lv

and string_of_bexp b = 
  match b with
  | True           -> "true" 
  | False          -> "false"
  | Equal (a1, a2) -> string_of_exp a1 ^ " == " ^ string_of_exp a2
  | Le    (a1, a2) -> string_of_exp a1 ^ " <= " ^ string_of_exp a2
  | Not b          -> "!(" ^ string_of_bexp b ^ ")"
  | And   (b1, b2) -> string_of_bexp b1 ^ " && " ^ string_of_bexp b2

module type Node = sig
  type instr = 
  | I_assign of lv * exp 
  | I_assume of bexp 
  | I_alloc of lv
  | I_skip
  type t = int * instr 
  val create_assign : lv -> exp -> t 
  val create_assume : bexp -> t 
  val create_alloc : lv -> t 
  val create_skip : unit -> t 
  val get_nodeid : t -> int 
  val get_instr : t -> instr 
  val to_string : t -> string
  val compare : t -> t -> int   
end

module Node : Node = struct
  type instr = 
  | I_assign of lv * exp 
  | I_assume of bexp 
  | I_alloc of lv  
  | I_skip
  type t = int * instr
  let new_id : unit -> int =
    let id =  ref 0 in 
      fun _ -> (id := !id + 1; !id)
  let create_assign x a = (new_id(), I_assign (x, a))
  let create_assume b = (new_id(), I_assume b)
  let create_alloc l = (new_id(), I_alloc l)
  let create_skip () = (new_id(), I_skip)
  let get_nodeid (id, _) = id
  let get_instr (_, instr) = instr
  let compare = Stdlib.compare
  let to_string n = 
    match n with
    | (id, I_assign (x, a)) -> 
      string_of_int id ^ ": " ^  string_of_lv x ^ " := " ^ string_of_exp a
    | (id, I_assume b) -> 
      string_of_int id ^ ": " ^ "assume"  ^ " " ^ string_of_bexp b
    | (id, I_alloc lv) ->
      string_of_int id ^ ": " ^ string_of_lv lv ^ " := " ^ "alloc" 
    | (id, I_skip) -> 
      string_of_int id ^ ": " ^ "skip"
end

module NodeSet = Set.Make(Node)
module NodeMap = Map.Make(Node)

module type Cfg = sig 
  type t 
  val empty : t 
  val nodesof : t -> Node.t list 
  val succs : Node.t -> t -> NodeSet.t
  val preds : Node.t -> t -> NodeSet.t
  val add_node : Node.t -> t -> t
  val add_nodes : Node.t list -> t -> t
  val add_edge : Node.t -> Node.t -> t -> t
  val print : t -> unit 
  val dot : t -> unit
end 

module Cfg : Cfg = struct
  (* cfg itself is one structure that consists of diverse nodes. *)
  type t = { 
    nodes : NodeSet.t; 
    succs : NodeSet.t NodeMap.t; (* mapping from current node to succeding node. *) 
    preds : NodeSet.t NodeMap.t }
  let empty = { 
    nodes = NodeSet.empty; 
    succs = NodeMap.empty; 
    preds = NodeMap.empty }

  (* return nodes *)
  let nodesof : t -> Node.t list 
  =fun t -> NodeSet.elements t.nodes

  (* return succ of input node *)
  let succs : Node.t -> t -> NodeSet.t
  =fun n g -> try NodeMap.find n g.succs with _ -> NodeSet.empty

  (**)
  let preds : Node.t -> t -> NodeSet.t
  =fun n g -> try NodeMap.find n g.preds with _ -> NodeSet.empty

  let add_node : Node.t -> t -> t
  (* with means updating nodes only*)
  =fun n g -> { g with nodes = NodeSet.add n g.nodes }

  let add_nodes : Node.t list -> t -> t
  =fun ns g -> g |> (List.fold_right add_node ns)
    
  let (|>) x f = f x
  let add_edge : Node.t -> Node.t -> t -> t
  =fun n1 n2 g -> 
    g 
    |> add_nodes [n1;n2] 
    (* it means n1 -> Succ of n1 Set(n2, ...) *)
    |> (fun g -> { g with 
          succs = NodeMap.add n1 (NodeSet.add n2 (succs n1 g)) g.succs }) 
    |> (fun g -> { g with 
          preds = NodeMap.add n2 (NodeSet.add n1 (preds n2 g)) g.preds }) 

  let print g = 
    print_endline "** Nodes **";
    NodeSet.iter (fun n -> 
      print_endline (Node.to_string n)
    ) g.nodes;
    print_endline "";
    print_endline "** Edges **";
    NodeMap.iter (fun n succs -> 
      NodeSet.iter (fun s ->
        print_endline (string_of_int (Node.get_nodeid n) ^ " -> " ^ 
          string_of_int (Node.get_nodeid s))
      ) succs
    ) g.succs

  let dot g = 
    print_endline "digraph G {";
    NodeSet.iter (fun n -> 
      print_string (string_of_int (Node.get_nodeid n) ^ " ");
      print_string ("[label=\"" ^ Node.to_string n ^ "\"]");
      print_endline ""
    ) g.nodes;
    NodeMap.iter (fun n succs -> 
      NodeSet.iter (fun s ->
        print_endline (string_of_int (Node.get_nodeid n) ^ " -> " ^ 
          string_of_int (Node.get_nodeid s))
      ) succs
    ) g.succs;
    print_endline "}"
end

(* 
  Challenge 1.

  1. when you meet "Seq", you need to make two "skip"s, the before one is starting point, the after one is end point.   
  2. when you meet "While", also you need to make two "skips" however, the before one is for both(loop and termition) and the latter one is just for termination
  3. in while, skip node, that is starting point, need to point two assume inst. in other case, pointing(using edge) is trivial.
  
 lp, ep means loop point and end point
 pr means pred
 loop means in loop or not.
*)

let take_pgm : cmd -> cmd list 
= function | Seq(lst) -> lst | _ -> (raise (Failure "Impossible case"))

(* Additional functiosn to get name in setting of hw2 *)
(* let get_var_name : lv -> string  *)
(* = function | Var x -> x | Ptr  *)
(* done *)

let rec seperate_bool : bexp -> Node.t -> Node.t list -> Cfg.t -> (Node.t * Node.t list * Cfg.t)
= fun bexp sp lst cfg -> match bexp with 
  | True           -> let suss = (Node.create_assume bexp) in (suss, lst, (Cfg.add_edge sp suss cfg))  
  | False          -> let fail = (Node.create_assume bexp) in (fail, lst, (Cfg.add_edge sp fail cfg)) 
  | Equal (a1, a2) -> let suss = Node.create_assume bexp in let fail = Node.create_assume (Not bexp) in (suss ,fail::lst, (Cfg.add_edge sp suss cfg))
  | Le    (a1, a2) -> let suss = Node.create_assume bexp in let fail = Node.create_assume (Not bexp) in (suss, fail::lst, (Cfg.add_edge sp suss cfg))
  | Not   b1       -> let suss = Node.create_assume bexp in let fail = Node.create_assume (Not bexp) in (suss, fail::lst, (Cfg.add_edge sp suss cfg))
  | And   (b1, b2) -> let (suss, lst1, cfg1) = seperate_bool b1 sp lst cfg in let (suss', lst2, cfg2) = seperate_bool b2 suss lst1 cfg1 in (suss', lst2, cfg2) 

let rec cmd2cfg : cmd -> Cfg.t 
= fun cmd -> let init = Cfg.empty in 
  let lst = take_pgm cmd in 
  let sp' = Node.create_skip() in let ep' = Node.create_skip() in (parsing lst init sp' ep' sp' false) 
and parsing : cmd list -> Cfg.t -> Node.t -> Node.t -> Node.t -> bool -> Cfg.t   
= fun smt cfg lp ep pr loop -> match smt with
 | hd::tl -> begin match hd with 
    | Assign (s1, a2)      -> let cur = Node.create_assign s1 a2 in Cfg.add_edge pr cur (parsing tl cfg lp ep cur loop)
    | Alloc  (lv)          -> let cur = Node.create_alloc lv in Cfg.add_edge pr cur (parsing tl cfg lp ep cur loop)
    | Seq    (l1)          -> let sp' = Node.create_skip() in let ep' = Node.create_skip() in  
                              let cfg' = Cfg.add_edge pr sp' cfg in 
                              if loop then parsing l1 cfg' lp ep' sp' loop 
                              else let cfg'' = Cfg.add_edge ep' ep cfg' in parsing l1 cfg'' lp ep' sp' loop 
    | If     (b1, c2, c3)  -> let sp' = Node.create_skip() in let ep' = Node.create_skip() in
                              let pass = Node.create_assume b1 in let npass = Node.create_assume(Not (b1)) in 
                              let cfg' = Cfg.add_edge pr sp' cfg in 
                              let cfg'' = Cfg.add_edge sp' pass cfg' in 
                              let cfg''' = (parsing [c2] cfg'' lp ep' pass false) in (* this seq is not in loop *) 
                              let cfg'''' = Cfg.add_edge sp' npass cfg''' in 
                              let cfg''''' = (parsing [c3] cfg'''' lp ep' npass false) in 
                              parsing tl cfg''''' lp ep ep' loop  
    | While  (b1, c2)      -> let sp' = Node.create_skip() in let ep' = Node.create_skip() in
                              let pass = Node.create_assume b1 in let npass = Node.create_assume(Not (b1)) in  
                              let cfg = Cfg.add_edge pr sp' cfg in (* connect 4 to 5*)
                              let cfg' = Cfg.add_edge sp' pass cfg in 
                              let cfg'' = (parsing [c2] cfg' sp' ep' pass true) in 
                              let cfg''' = Cfg.add_edge sp' npass cfg'' in
                              let cfg'''' = (parsing tl cfg''' lp ep' npass loop) in 
                              if loop then cfg'''' else Cfg.add_edge ep' ep cfg'''' (* connect 10 to 5 *) 
    | Skip                 -> let cur = Node.create_skip() in Cfg.add_edge pr cur (parsing tl cfg lp ep cur loop)
  end
  | _ -> let cfg' = Cfg.add_edge pr ep cfg in if loop then Cfg.add_edge ep lp cfg' else cfg'
(****)

module type AbsBool = sig
  type t = Top | Bot | True | False 
  val not : t -> t 
  val band : t -> t -> t
end

module AbsBool : AbsBool = struct
  type t = Top | Bot | True | False
  let not : t -> t
  = fun b -> match b with 
    | True -> False 
    | False -> True
    | _ -> b
  let band : t -> t -> t
  = fun b1 b2 -> match b1, b2 with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot 
    | False, _ -> False
    | _, False -> False 
    | Top, _ -> Top
    | _, Top -> Top
    | True, True -> True (* it is the other case.*)
end

(* Added in hw2*)
module BaseLoc = struct
  type t = 
    | Var of string 
    | Allsite of int (* the number of node *) 

  let compare : t -> t -> int 
  = compare

  let to_string : t -> string 
  = fun a -> match a with 
    | Var s     -> "Var " ^ s 
    | Allsite i -> "Allsite " ^ string_of_int i 
end

module type AbsLoc = sig
  type t = BaseLoc.t list 

  val compare : 'a -> 'a -> int

  val string_of_atom : BaseLoc.t -> string
  val to_string : bool -> t -> string -> string 

  val order : t -> t -> bool 
  val join : t -> t -> t
  val meet : t -> t -> t -> t 
  (* val widen : t -> t -> t *)
  (* val narrow : t -> t -> t *)
end

(* TODO *)
module AbsLoc : AbsLoc = struct 
  type t = BaseLoc.t list

  let compare = compare

  let string_of_atom : BaseLoc.t -> string 
  = fun a -> match a with 
    | Var s     -> "Var " ^ s 
    | Allsite i -> "Allsite " ^ string_of_int i 

  (* the first boolean argument is a flague to know whether it is the first to enter or not. *)
  let rec to_string : bool -> t -> string -> string
  = fun isFirst lst str -> match lst with 
    | hd::tl -> let str' = str ^ string_of_atom hd ^ "," in to_string false tl str' 
    | _      -> if isFirst then "Bottom" else str

  let rec order l1 l2 = match l1 with 
    | hd::tl -> if (List.mem hd l2) then order tl l2 else false 
    | _      -> true 
    
  let rec join l1 l2 = match l1 with  
    | hd::tl -> if (List.mem hd l2) then join tl l2 else join tl (hd::l2)
    | _      -> l2 

  let rec meet l1 l2 lst = match l1 with  
    | hd::tl -> if (List.mem hd l2) then meet tl l2 (hd::lst) else meet tl l2 lst 
    | _      -> lst 

end

module type Value = sig
  type atom = Con of int | N_inf | P_inf
  type t    = Bot | Top | Iv of atom * atom | Var of string | Allsite of int    
  val bottom : t
  val to_string : t -> string
  val alpha : int -> t 
  val alpha_to : int -> t
  val alpha_from : int -> t
  val order : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val sub : t -> t -> t
  val equal : t -> t -> AbsBool.t
  val le : t -> t -> AbsBool.t
  val ge : t -> t -> AbsBool.t
  val comp : atom -> atom -> bool
end

module Value : Value = struct
  type atom = Con of int | N_inf | P_inf
  (* N_inf means negative infinite, P_int means postiive negative.*)
  type t = Bot | Top | Iv of atom * atom | Var of string | Allsite of int 

  let bottom = Bot 

  let string_of_atom : atom -> string
  = fun a -> match a with 
    | Con i1 -> string_of_int i1 
    | N_inf -> "-oo"
    | P_inf -> "+oo"

  let to_string : t -> string 
  = fun i -> match i with 
    | Bot        -> "Bottom" 
    | Top        -> "Top"
    | Iv(i1, i2) -> let str_i1 = string_of_atom i1 in let str_i2 = string_of_atom i2 in "[" ^ str_i1 ^ ", " ^ str_i2 ^ "]"
    | Var s      -> "Var " ^ s 
    | Allsite i  -> "AllocSite " ^ string_of_int i 

  (* I guess this function is for abstracting concrete values. *)
  let alpha : int -> t 
  = fun n -> Iv(Con n, Con n)  

  (* (-inf, n) *)
  let alpha_to : int -> t 
  = fun n -> Iv(N_inf, Con n)

  (* (n, inf) *)
  let alpha_from : int -> t
  = fun n -> Iv(Con n, P_inf)

  (* order for atom. only if something located in right is greater than left one, then return value would be true*)
  let comp : atom -> atom -> bool 
  = fun a b -> match a, b with 
    | _, P_inf -> true
    | P_inf, _ -> false 
    | N_inf, _ -> true 
    | _, N_inf -> false 
    | Con x, Con y -> if x <= y then true else false 

  let interval_min = fun a b -> if comp a b then a else b 

  let interval_max = fun a b -> if comp a b then b else a 

  (* okay *)
  let order : t -> t -> bool 
  = fun a b -> match (a,b) with 
    | Bot, _ -> true 
    | _, Bot -> false
    | _, Top -> true
    | Top, _ -> false  
    (* interval cases. thinks about inf cases frist!*)
    | Iv(a1, a2), Iv(b1, b2) -> if (comp b1 a1) && (comp a2 b2) then true else false 
    | _ -> raise (Failure "undefined")

  (* okay *)
  let join a b = match a, b with
    | Bot, y -> y
    | x, Bot -> x
    | Top, _ -> Top 
    | _, Top -> Top 
     (* interval cases. thinks about inf cases frist!*)
    | Iv(a1, a2), Iv(b1, b2) -> if      (comp a1 b1) && (comp a2 b2) then Iv(a1, b2) 
                                else if (comp a1 b1) && (comp b2 a2) then Iv(a1, a2)
                                else if (comp b1 a1) && (comp a2 b2) then Iv(b1, b2)
                                else if (comp b1 a1) && (comp b2 a2) then Iv(b1, a2)
                                else raise (Failure "Error: Impossible case in join")
    | _ -> raise (Failure "undefined")

  (* okay *)
  let meet a b = match (a, b) with  
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, y -> y 
    | x, Top -> x 
    | Iv(a1, a2), Iv(b1, b2) -> if (comp a1 b1) && (comp a2 b2) then if comp b1 a2 then Iv(b1, a2) else Bot (* intersection case *)
                                            else if (comp a1 b1) && (comp b2 a2) then Iv(b1, b2) (* this case is in situation where b is included in a.*)
                                            else if (comp b1 a1) && (comp a2 b2) then Iv(a1, a2) (* this case is in situation where a is included in b.*)
                                            else if (comp b1 a1) && (comp b2 a2) then if comp a1 b2 then Iv(a1, b2) else Bot (* intersection case *)
                                            else raise (Failure "Error: Impossible case in meet")
    | _ -> raise (Failure "undefined")
 
  (* this will be executed on x (widen) f(x). *)
  let widen a b = match (a, b) with  
    | Bot, y -> y
    | x, Bot -> x
    | Top, _ -> Top (* need to know more *) 
    | _, Top -> Top (* need to know more *)
    | Iv(a1, a2), Iv(b1, b2) -> let new_a = if (comp a1 b1) then a1 else N_inf in 
                                let new_b = if (comp b2 a2) then a2 else P_inf in 
                                Iv(new_a, new_b)
    | _ -> raise (Failure "undefined")
 
  let narrow a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, y -> y (* need to know more *) 
    | x, Top -> x (* need to know more *)
    | Iv(a1, a2), Iv(b1, b2) -> let new_a = if (a1 = N_inf) then b1 else a1 in 
                                let new_b = if (a2 = P_inf) then b2 else a2 in 
                                Iv(new_a, new_b)
    | _ -> raise (Failure "undefined")
 
  (* okay *)
  let add a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, _ -> Top 
    | _, Top -> Top 
    | Iv(a1, a2), Iv(b1, b2) -> 
      let new_a = (match a1, b1 with 
        | P_inf, _ -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | _, P_inf -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | N_inf, _ -> N_inf 
        | _, N_inf -> N_inf 
        | Con a1', Con b1' -> Con (a1' + b1')) in 
      let new_b = (match a2, b2 with 
        | N_inf, _ -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *) 
        | _, N_inf -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *)
        | P_inf, _ -> P_inf
        | _, P_inf -> P_inf
        | Con a2', Con b2' -> Con (a2' + b2')) in 
        Iv(new_a, new_b) 
    | _ -> raise (Failure "undefined")

  (* okay *)
  let sub a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, _ -> Top 
    | _, Top -> Top 
    | Iv(a1, a2), Iv(b1, b2) -> 
      let new_a = (match a1, b2 with (* minimum case: a1 - b2 *) 
        | P_inf, _ -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | _, N_inf -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *)
        | N_inf, _ -> N_inf 
        | _, P_inf -> N_inf 
        | Con a1', Con b1' -> Con (a1' - b1')) in 
      let new_b = (match a2, b1 with (* maximum case: a2 - b1 *) 
        | N_inf, _ -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *) 
        | _, P_inf -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | P_inf, _ -> P_inf
        | _, N_inf -> P_inf
        | Con a2', Con b2' -> Con (a2' - b2')) in 
        Iv(new_a, new_b) 
    | _ -> raise (Failure "undefined")

  (* in this part, we should consider order between new_a and new_b *)
  (*
    input: [a, b] [c, d]
  
    1) caculate a*c, a*d, b*c, b*d. 
     1-1) if there is error case, handle it.
    2) find min and max.
    3) return [min, max]
  *)
  let rec mul a b = match (a, b) with  
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    (* | Iv(Con 0, Con 0), _ -> Iv(Con 0, Con 0)
    | _, Iv(Con 0, Con 0) -> Iv(Con 0, Con 0) *)
    | Top, _ -> Top 
    | _, Top -> Top 
    | Iv(a1, a2), Iv(b1, b2) -> let t1 = mul_aux a1 b1 in let t2 = mul_aux a1 b2 in let t3 = mul_aux a2 b1 in let t4 = mul_aux a2 b2 in 
                                            let candidate = [t1;t2;t3;t4] in 
                                            let c1 = List.fold_right interval_min candidate t1 in 
                                            let c2 = List.fold_right interval_max candidate t1 in
                                            Iv(c1, c2)
    | _ -> raise (Failure "undefined")
  and mul_aux : atom -> atom -> atom 
  = fun a b -> match a, b with 
    | P_inf, z' | z', P_inf -> (match z' with
                                | P_inf -> P_inf
                                | N_inf -> N_inf
                                | Con 0 -> Con 0 
                                | Con n -> if 0 < n then P_inf else N_inf)
    | N_inf, z' | z', N_inf ->  (match z' with
                                | P_inf -> N_inf
                                | N_inf -> P_inf
                                | Con 0 -> Con 0 
                                | Con n -> if 0 < n then N_inf else P_inf)
    | Con a', Con b' -> Con (a' * b') 
  (* Need to consider n' = Interval.alpha  2 inclusion cases and 2 intersection cases. *)
  let rec intersection : t -> t -> bool
  = fun a b -> match a, b with 
    | Iv(a1, a2), Iv(b1, b2) -> 
      if comp a1 b1 then (if comp b2 a2 then true else (if comp b1 a2 then true else false))
      else if comp b1 a1 then (if comp a2 b2 then true else (if comp a1 b2 then true else false))
      else false 
    | _ -> false

  (* 
     True  -> Only if a1 = a2 = a3 = a4 in Con. 
     False -> case for no intersection
     Top   -> otherwise. it means there is an intersection.
  *)
  let equal a b = match (a, b) with
    | Bot, _ -> AbsBool.Bot 
    | _, Bot -> AbsBool.Bot  
    | Top, _ -> AbsBool.Top 
    | _, Top -> AbsBool.Top 
    | Iv(Con a1, Con a2), Iv(Con b1, Con b2) -> 
      if (a1 = a2) && (a2 = b1) && (b1 = b2) then AbsBool.True
      else if intersection a b then AbsBool.Top else AbsBool.False
    | x, y -> if intersection x y then AbsBool.Top else AbsBool.False 

(*
    When we talk based on location of b,
    True  -> No intersection, only if range is left compared to 'a' perfectly.
    False -> No intersection, only if range is right compare to 'a' perfectly.
    Top -> interesection.
*)
  let le a b = match (a,b) with 
    | Bot, _ -> AbsBool.Bot
    | _, Bot -> AbsBool.Bot
    | Top, _ -> AbsBool.Top
    | _, Top -> AbsBool.Top
    | Iv(a1, a2), Iv(b1, b2) -> 
      if comp a2 b1 then AbsBool.True else (* this one includes a2 == b1 case.*)
        if intersection a b then AbsBool.Top else AbsBool.False  (* b1 is less than a2 without intersection. it means whole range of b is less then range of a. *)
    | _ -> raise (Failure "undefined")

  let ge a b = match (a,b) with   
    | Bot, _ -> AbsBool.Bot
    | _, Bot -> AbsBool.Bot
    | Top, _ -> AbsBool.Top
    | _, Top -> AbsBool.Top
    | Iv(a1, a2), Iv(b1, b2) -> 
      if comp b2 a1 then AbsBool.True else (* this one includes a2 == b1 case.*)
        if intersection a b then AbsBool.Top else AbsBool.False  (* b1 is less than a2 without intersection. it means whole range of b is less then range of a. *)
    | _ -> raise (Failure "undefined")
end

module VarMap = Map.Make(BaseLoc)
module type AbsMem = sig
  type value = (AbsLoc.t * Value.t)
  type t     = (AbsLoc.t * Value.t) VarMap.t 
  val empty : t
  val add : BaseLoc.t -> value -> t -> t
  val find : BaseLoc.t -> t -> value 
  val join : t -> t -> t 
  val meet : t -> t -> t 
  val widen : t -> t -> t 
  val narrow : t -> t -> t
  val order : t -> t -> bool 
  val print : t -> unit 
end

(* this one is for caculation while executing commands with absmem *)
module AbsMem : AbsMem = struct
  type value = (AbsLoc.t * Value.t)
  type t     = value VarMap.t 

  let print m = VarMap.iter (fun x (l, v) -> prerr_endline 
    (BaseLoc.to_string x ^ " |-> " ^ (AbsLoc.to_string true l "") ^ " | " ^ (Value.to_string v))) m 

  let empty = VarMap.empty

  let find x m = try VarMap.find x m with _ -> ([], Value.bottom) 

  let add x v m = VarMap.add x v m  

  let keys b = List.fold_right (fun (k,v) lst -> k::lst) b []

  let rec union_keys : BaseLoc.t list -> BaseLoc.t list -> BaseLoc.t list 
  = fun a b -> match b with 
    | hd::tl -> if List.mem hd a then union_keys a tl else union_keys (hd::a) tl 
    | [] -> a 

  let rec calc : (value -> value -> value) -> t -> t -> BaseLoc.t list -> t -> t
  = fun f m1 m2 keys m3 -> match keys with
    | hd::tl -> let m2_val = find hd m2 in let m1_val = find hd m1 in 
                let m3' = add hd (f m1_val m2_val) m3 in
                calc f m1 m2 tl m3'
    | _ -> m3 

  let rec calc_bool : (value -> value -> bool) -> t -> t -> BaseLoc.t list -> bool 
  = fun f m1 m2 keys -> match keys with
    | hd::tl -> let m2_val = find hd m2 in let m1_val = find hd m1 in 
                if (f m1_val m2_val) then calc_bool f m1 m2 tl else false   
    | _ -> true 
  
  (* m2 is bigger than m1 *)
  let rec join m1 m2 = 
    (* let _  = print_endline __FUNCTION__ in  *)
    let m1' = VarMap.bindings m1 in 
    let m2' = VarMap.bindings m2 in 
    let m1_keys = keys m1' in
    let m2_keys = keys m2' in  
    let keys = union_keys m1_keys m2_keys in 
    let m3 = empty in calc join_aux m1 m2 keys m3 
  and join_aux : value -> value -> value 
  = fun a b -> let (a_loc, a_iv) = a in let (b_loc, b_iv) = b in 
  let a' = AbsLoc.join a_loc b_loc in let b' = Value.join a_iv b_iv in (a', b') 
    
   let rec meet : t -> t -> t 
   = fun m1 m2 -> 
    let m1' = VarMap.bindings m1 in 
    let m2' = VarMap.bindings m2 in 
    let m1_keys = keys m1' in
    let m2_keys = keys m2' in  
    let keys = union_keys m1_keys m2_keys in 
    let m3 = empty in calc (meet_aux) m1 m2 keys m3  
  and meet_aux : value -> value -> value 
  = fun a b -> let (a_loc, a_itv) = a in let (b_loc, b_itv) = b in 
  let a' = AbsLoc.meet a_loc b_loc [] in let b' = Value.meet a_itv b_itv in (a', b') 

  let rec widen m1 m2 = 
    (* let _  = print_endline __FUNCTION__ in  *)
    let m1' = VarMap.bindings m1 in 
    let m2' = VarMap.bindings m2 in 
    let m1_keys = keys m1' in
    let m2_keys = keys m2' in  
    let keys = union_keys m1_keys m2_keys in 
    let m3 = empty in calc (widen_aux) m1 m2 keys m3 
  and widen_aux : value -> value -> value 
  = fun a b -> let (a_loc, a_itv) = a in let (b_loc, b_itv) = b in 
  let a' = AbsLoc.join a_loc b_loc in let b' = Value.widen a_itv b_itv in (a', b') 

  let rec narrow m1 m2 = 
    (* let _  = print_endline __FUNCTION__ in  *)
    let m1' = VarMap.bindings m1 in 
    let m2' = VarMap.bindings m2 in 
    let m1_keys = keys m1' in
    let m2_keys = keys m2' in  
    let keys = union_keys m1_keys m2_keys in 
    let m3 = empty in calc (narrow_aux) m1 m2 keys m3 
  and narrow_aux : value -> value -> value 
  = fun a b -> let (a_loc, a_itv) = a in let (b_loc, b_itv) = b in 
  let a' = AbsLoc.meet a_loc b_loc [] in let b' = Value.narrow a_itv b_itv in (a', b') 

  (* only if all cases are true. *)
  let rec order m1 m2 = 
    (* let _  = print_endline __FUNCTION__ in  *)
    let m1' = VarMap.bindings m1 in 
    let m2' = VarMap.bindings m2 in 
    let m1_keys = keys m1' in
    let m2_keys = keys m2' in  
    let keys = union_keys m1_keys m2_keys in 
    calc_bool (order_aux) m1 m2 keys
    and order_aux : value -> value -> bool 
    = fun a b -> let (a_loc, a_itv) = a in let (b_loc, b_itv) = b in 
    let a' = AbsLoc.order a_loc b_loc in let b' = Value.order a_itv b_itv in 
    if (a' = true) && (b' = true) then true else false 
end

module type Table = sig
  type t = AbsMem.t NodeMap.t
  val empty : t
  val add : Node.t -> AbsMem.t -> t -> t
  val init : Node.t list -> t 
  val find : Node.t -> t:t -> AbsMem.t 
  val print : t -> unit
end 

module Table : Table = struct 
  type t = AbsMem.t NodeMap.t
  let empty = NodeMap.empty 
  let add = NodeMap.add
  let init ns = List.fold_right (fun n -> add n AbsMem.empty) ns empty
  let find : Node.t -> t:t -> AbsMem.t 
  =fun n ~t -> try NodeMap.find n t with _ -> AbsMem.empty
  let print t = NodeMap.iter (fun n m -> 
    prerr_endline (string_of_int (Node.get_nodeid n)); 
    AbsMem.print m; 
    prerr_endline "") t  
end

(* I could have used 'meet' but I wrote additional function. *)
let handle_le : Value.t -> int -> bool -> Value.t
= fun iv i ord -> 
  let ii = Value.Con i in 
  let ip = Value.Con(i+1) in
  let im = Value.Con(i-1) in
  match iv with (* if ord is true, then x <= iv, else iv <= x *)
  | Value.Bot -> Value.Bot
  | Value.Top -> if ord then Value.Iv(Value.N_inf, (Value.Con i)) else Value.Iv((Value.Con i), Value.P_inf)
  | Value.Iv(i1, i2) -> if ord then (if (Value.comp ip i1) then Value.Bot else (if (Value.comp i2 im) then iv else Value.Iv(i1, ii))) 
                                  else (if (Value.comp i2 im) then Value.Bot else (if (Value.comp ip i1) then iv else Value.Iv(ii, i2)))
    | _ -> raise (Failure "undefined")

let update_option a = function | None -> Some a | Some _ -> Some a  


let rec convert_loc : lv -> BaseLoc.t
= fun lv -> match lv with 
  | Var s -> BaseLoc.Var s
  | _     -> raise (Failure "this function cannot be applied to ptr")

let get_loc = function | BaseLoc.Var s -> s | Allsite _ -> raise (Failure "impossible case")

(* check location or arithmetic value *)
let check_location : Value.t -> bool (* return true when it is location*) 
= fun va -> match va with 
  | Bot -> true   
  | Top -> true
  | Iv (a1, a2) -> false 
  | Var s -> true 
  | Allsite i -> true 

let rec ptr_to_var : lv -> lv
= fun lv -> match lv with 
  | Var _ -> lv
  | Ptr s -> ptr_to_var s

let rec weak_update : BaseLoc.t list -> AbsMem.t -> AbsMem.value -> AbsMem.t
= fun loc mem value -> match loc with 
  | hd::tl -> let hd_mem: AbsMem.t = AbsMem.add hd value AbsMem.empty in let mem' = AbsMem.join hd_mem mem in weak_update tl mem' value  
  | _      -> mem

(* return ptr itself.*)
let rec join_loc : int -> lv -> AbsMem.t -> AbsLoc.t
= fun cnt lv mem -> match lv with
  | Ptr p -> join_loc (cnt+1) p mem
  | Var s -> let (loc', _) = AbsMem.find (BaseLoc.Var s) mem in loc_loop cnt loc' mem
and loc_aux : AbsLoc.t -> AbsMem.t -> AbsLoc.t
= fun lst mem -> match lst with                                 
  | hd::tl -> let (cur, _) = AbsMem.find hd mem in cur@(loc_aux tl mem)  
  | _      -> [] 
and loc_loop : int -> AbsLoc.t -> AbsMem.t -> AbsLoc.t  
= fun cnt lst mem -> if cnt = 0 then lst else loc_loop (cnt-1) (loc_aux lst mem) mem

(* return joined value of ptr*)
let rec join_ptr : BaseLoc.t list -> AbsMem.t -> Value.t -> AbsMem.value
= fun loc mem value -> match loc with 
  | hd::tl -> let (_, hd_val) = AbsMem.find hd mem in let new_val = Value.join hd_val value in join_ptr tl mem new_val 
  | _      -> ([], value)

(* int is the number of node. *)
let rec execute : int -> Node.instr -> AbsMem.t -> AbsMem.t
= fun idx cmd mem -> match cmd with 
  | I_assign (Var s, a2) -> let a2' = (execute_exp a2 mem) in 
                            VarMap.update (BaseLoc.Var s) (update_option a2') mem 
  | I_assign (Ptr s, a2) -> let a2' = (execute_exp a2 mem) in 
                            let loc' = join_loc (-1) (Ptr s) mem in 
                            let number_of_loc = List.length loc' in 
                            if number_of_loc = 1 
                            then VarMap.update (List.hd loc') (update_option a2') mem 
                            else weak_update loc' mem a2' 
  | I_skip     -> mem  
  | I_assume b -> execute_bexp b mem false  
  | I_alloc l  -> (* 1. Add new allocsite in l, 2. Add 0 in new location. *) 
                  let zero: AbsMem.value = ([], Value.alpha 0) in 
                  let addr: BaseLoc.t = BaseLoc.Allsite idx in 
                  let addr': AbsMem.value = ([addr], Value.Bot) in 
                  let loca: BaseLoc.t = convert_loc (ptr_to_var l) in
                  let mem': AbsMem.t  = VarMap.update addr (update_option zero) mem in 
                  let mem'': AbsMem.t = VarMap.update loca (update_option addr') mem' in mem'' 
and execute_exp : exp -> AbsMem.t -> AbsMem.value 
= fun exp mem -> match exp with 
  | Const i       -> ([], Value.alpha i)
  | Plus (a1, a2) -> let _, a1' = (execute_exp a1 mem) in let _, a2' = (execute_exp a2 mem) in
                     ([], Value.add a1' a2')
  | Mult (a1, a2) -> let _, a1' = (execute_exp a1 mem) in let _, a2' = (execute_exp a2 mem) in
                     ([], Value.mul  a1' a2')
  | Sub  (a1, a2) -> let _, a1' = (execute_exp a1 mem) in let _, a2' = (execute_exp a2 mem) in
                     ([], Value.sub a1' a2')
  | Lv   (Ptr p)  -> let s = ptr_to_var p in let s' = convert_loc s in let loc, mem' = AbsMem.find s' mem in join_ptr loc mem Value.bottom
  | Lv   (Var s)  -> let s' = BaseLoc.Var s in AbsMem.find s' mem 
  | Loc  (Var s)  -> ([BaseLoc.Var s], Value.Bot)
  | Loc  _        -> raise (Failure "undefined") 
(* this one will return memory itself.*)
and execute_bexp : bexp -> AbsMem.t -> bool -> AbsMem.t
= fun exp mem not -> match exp with (* n_not means the number of not *) 
  | True           -> if not then AbsMem.empty else mem 
  | False          -> if not then mem else AbsMem.empty
  | Equal (a1, a2) -> (match a1, a2 with
                      | Const n1, Const n2 -> if not then (if n1 = n2 then AbsMem.empty else mem) else (if n1 = n2 then mem else AbsMem.empty)
                      | Const n, Lv _ -> execute_bexp (Equal(a2, a1)) mem not 
                      | Const n, Loc _ -> execute_bexp (Equal(a2, a1)) mem not 
                      | Lv s, Const n -> let s' = convert_loc s in let _, temp = AbsMem.find s' mem in
                        if not 
                        then  let output = (match temp with 
                                | Iv (Con a, Con b) -> if (a = n) && (b = n) then Value.Bot else(
                                                        if a = b then temp 
                                                        else if a = n then Iv (Value.Con (a+1), Value.Con b)
                                                        else if b = n then Iv (Value.Con a, Value.Con (b-1))  
                                                        else temp)
                                | Iv (N_inf, Con b) -> if b = n then Iv (N_inf, Con (b-1)) else temp
                                | Iv (Con a, P_inf) -> if a = n then Iv (Con (a+1), P_inf) else temp
                                | Iv (N_inf, P_inf) -> temp
                                | Bot -> Bot
                                | Top -> Top
                                | _ -> raise (Failure "Impossible case in not equal case.")) 
                                in VarMap.update (convert_loc s) (update_option ([], output)) mem  
                        else let new_val = Value.meet temp (Value.alpha n) in VarMap.update s' (update_option ([], new_val)) mem 
                      | Lv s1, Lv s2 -> let _, s1' = VarMap.find (convert_loc s1) mem in let _, s2' = VarMap.find (convert_loc s2) mem in 
                                          if not then mem 
                                          else (let new_val = Value.meet s1' s2' in let mem' = VarMap.update (convert_loc s1) (update_option ([], new_val)) mem 
                                               in VarMap.update (convert_loc s2) (update_option ([], new_val)) mem')
                      | _, _ -> raise (Failure "Impossible case in not equal case."))  
  | Le    (a1, a2) -> (match a1, a2 with
                      | Const n1, Const n2 -> mem (* bottom? or normal execution? *)
                      | Const n, Lv s -> let s' = convert_loc s in 
                        if not then execute_bexp (Le(a2, Const(n-1))) mem false  
                               else let _, t1 = (AbsMem.find s' mem) in let new_iv = (handle_le t1 n false) in VarMap.update s' (update_option ([], new_iv)) mem 
                      | Lv s, Const n -> let s' = convert_loc s in
                        if not then execute_bexp (Le(Const(n+1), a1)) mem false  
                               else let _, t1 = (AbsMem.find s' mem) in let new_iv = (handle_le t1 n true) in VarMap.update s' (update_option ([], new_iv)) mem
                      | _, _ -> raise (Failure "Impossible case in not equal case."))
  | Not   b        -> execute_bexp b mem (if not then false else true) (* then -> double negation. *)  
  | And   (b1, b2) -> if not then let mem' = execute_bexp b1 mem not in let mem'' = execute_bexp b2 mem not in AbsMem.join mem' mem''  
                      else let temp1 = execute_bexp b1 mem not in let temp2 = execute_bexp b2 mem not in (AbsMem.meet temp1 temp2)
and update_aux : (exp * Value.t * bool) -> isEqual:bool -> mem:AbsMem.t -> (string * Value.t) 
= fun (var, iv, isLeft) ~isEqual ~mem -> let name = (function | Lv (Var s) -> s | _ -> raise (Failure "Error in name")) var in 
  if isEqual then (let temp = eq_aux name iv mem in (name, temp)) 
  else (let temp = le_aux name iv isLeft mem in (name, temp))   
and eq_aux : string -> Value.t -> AbsMem.t -> Value.t
= fun name iv mem -> let (_, s_value) = AbsMem.find (BaseLoc.Var name) mem in match s_value, iv with 
  | Bot, _ -> Bot 
  | _, Bot -> Bot 
  | Top, b -> b 
  | _, Top -> Top
  | a, b   -> Value.meet a b 
(* need unit test. *)
and le_aux : string -> Value.t -> bool -> AbsMem.t -> Value.t 
= fun name iv isLeft mem -> let (_, s_value) = AbsMem.find (BaseLoc.Var name) mem in 
    if isLeft then 
    (match (s_value, iv) with  
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, Top -> Top 
    | Top, Iv(a,b) -> Iv(N_inf, b) 
    | Iv(a,b), Top -> s_value 
    | Iv(a1,a2), Iv(b1,b2) -> (if not(Value.comp a1 b2) then Bot else (
                              let new_a = a1 in  
                              let new_b = if (Value.comp a2 b2) then a2 else b2 in Iv(new_a, new_b)))
    | _, _ -> raise (Failure "undefined"))
    else 
    (match (iv, s_value) with  
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, Top -> Top 
    | Top, Iv(a,b) -> Top 
    | Iv(a,b), Top -> Iv(a, P_inf) 
    | Iv(a1,a2), Iv(b1,b2) -> if not(Value.comp a1 b2) then Bot else (
                              let new_a = if (Value.comp a1 b1) then b1 else a1 in 
                              let new_b = b2 in Iv(new_a, new_b))
    | _, _ -> raise (Failure "undefined"))

let rec first_fhat : Node.t list -> Cfg.t -> Table.t -> Table.t
= fun lst cfg tab -> match lst with 
  | hd::tl -> let n, ins = hd in 
              let preds = NodeSet.elements (Cfg.preds (n,ins) cfg) in (* node of predecessors *) 
              let preds' = List.map (Table.find ~t:tab) preds in  (* AbsMem of predecessors *)
              (* let _ = print_endline ("[Pred of Node " ^ (string_of_int n) ^ "]" ^ (List.fold_right Node.to_string preds "")) in  *)
              let lub = List.fold_right AbsMem.join preds' AbsMem.empty in (* LUB of predecessors *)
              let s = execute n ins lub in (* applying f_hat *) 
              let n_tab = NodeMap.update hd (update_option s) tab in first_fhat tl cfg n_tab
  | _      -> tab 

let rec widening : Node.t list -> Cfg.t -> Table.t -> Table.t
= fun lst cfg tab -> match lst with 
  | hd::tl -> let n, ins = hd in 
              let preds = NodeSet.elements (Cfg.preds hd cfg) in (* node of predecessors *) 
              let preds' = List.map (Table.find ~t:tab) preds in  (* AbsMem of predecessors *)
              let lub = List.fold_right AbsMem.join preds' AbsMem.empty in (* LUB of predecessors *)
              let s = execute n ins lub in (* applying f_hat *) 
              let b_mem = Table.find hd ~t:tab in 
              if AbsMem.order s b_mem then widening tl cfg tab 
              else let n_mem = AbsMem.widen b_mem s in 
                   let n_tab = NodeMap.update hd (update_option n_mem) tab in 
                   let succs = NodeSet.elements (Cfg.succs hd cfg) in widening (tl@succs) cfg n_tab
  | _      -> tab 

let rec narrowing : Node.t list -> Cfg.t -> Table.t -> Table.t
= fun lst cfg tab -> match lst with 
  | hd::tl -> let n, ins = hd in 
              let preds = NodeSet.elements (Cfg.preds (n,ins) cfg) in (* node of predecessors *) 
              let preds' = List.map (Table.find ~t:tab) preds in  (* AbsMem of predecessors *)
              let lub = List.fold_right AbsMem.join preds' AbsMem.empty in (* LUB of predecessors *)
              let s = execute n ins lub in (* applying f_hat *) 
              let b_mem = Table.find hd ~t:tab in 
              if AbsMem.order b_mem s then narrowing tl cfg tab 
              else let n_mem = AbsMem.narrow b_mem s in let n_tab = NodeMap.update hd (update_option n_mem) tab in 
                   let succs = NodeSet.elements (Cfg.succs hd cfg) in narrowing (tl@succs) cfg n_tab
  | _      -> tab 

(* before starting widening and narrowing, need to add bottom as a predecessor of '1' node *)
let analyze : Cfg.t -> Table.t
= fun g -> let init_table = Table.init @@ Cfg.nodesof g in
  let worklist = Cfg.nodesof g in 
  let f_tab = first_fhat worklist g init_table in
  let res_of_widen = widening worklist g f_tab in 
  (* let _ = print_endline "result of widen"; Table.print res_of_widen in  *)
  let res_of_narrow = narrowing worklist g res_of_widen in 
  res_of_narrow


let pgm1 = 
  Seq [
    Assign ((Var "x"), Const 1); 
    Assign ((Var "p"), Loc (Var "x"));
    Assign ((Ptr (Var "p")), Plus (Lv(Ptr (Var "p")), Const 1))
  ]

let pgm2 = 
  Seq [
    Alloc (Var "p"); 
    Assign ((Var "q"), Loc(Var "p"));
    Assign ((Ptr (Ptr (Var "q"))), Const 1)
  ]

let pgm3 = 
  Seq [
    Assign ((Var "x"), Const 1); 
    While (Le (Lv(Var "x"), Const 9), 
      Seq [
        Alloc ((Var "p"));
        Assign ((Ptr(Var "p")), Plus (Lv(Ptr(Var "p")), Const 1)); 
        Assign ((Var "x"), Plus (Lv(Var "x"), Const 1)); 
      ]);
  ]
(* 
let cfg1 = cmd2cfg pgm1
let cfg2 = cmd2cfg pgm2 *)
let cfg3 = cmd2cfg pgm3
(* let _ = Cfg.print cfg *)
(* let _ = Cfg.dot cfg *)

(* let _ = print_endline "Program 1" 
let table1 = analyze cfg1 
let _ = Table.print table1

let _ = print_endline "Program 2" 
let table2 = analyze cfg2 
let _ = Table.print table2 *)

let _ = print_endline "Program 3" 
let table3 = analyze cfg3 
let _ = Table.print table3
