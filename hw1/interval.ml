type aexp = 
  | Const of int
  | Var of string
  | Plus of aexp * aexp
  | Mult of aexp * aexp
  | Sub of aexp * aexp

type bexp = 
  | True 
  | False
  | Equal of aexp * aexp
  | Le of aexp * aexp
  | Not of bexp
  | And of bexp * bexp

type cmd = 
  | Assign of string * aexp
  | Seq of cmd list
  | If of bexp * cmd * cmd
  | While of bexp * cmd

let rec string_of_aexp a = 
  match a with
  | Const n -> string_of_int n
  | Var x -> x
  | Plus (a1, a2) -> string_of_aexp a1 ^ " + " ^ string_of_aexp a2
  | Mult (a1, a2) -> string_of_aexp a1 ^ " * " ^ string_of_aexp a2
  | Sub (a1, a2) -> string_of_aexp a1 ^ " - " ^ string_of_aexp a2

and string_of_bexp b = 
  match b with
  | True -> "true" 
  | False -> "false"
  | Equal (a1, a2) -> string_of_aexp a1 ^ " == " ^ string_of_aexp a2
  | Le (a1, a2) -> string_of_aexp a1 ^ " <= " ^ string_of_aexp a2
  | Not b -> "!(" ^ string_of_bexp b ^ ")"
  | And (b1, b2) -> string_of_bexp b1 ^ " && " ^ string_of_bexp b2

module type Node = sig
  type instr = 
  | I_assign of string * aexp 
  | I_assume of bexp 
  | I_skip
  type t 
  val create_assign : string -> aexp -> t 
  val create_assume : bexp -> t 
  val create_skip : unit -> t 
  val get_nodeid : t -> int 
  val get_instr : t -> instr 
  val to_string : t -> string
  val compare : t -> t -> int   
end

module Node : Node = struct
  type instr = 
  | I_assign of string * aexp 
  | I_assume of bexp 
  | I_skip
  type t = int * instr
  let new_id : unit -> int =
    let id =  ref 0 in 
      fun _ -> (id := !id + 1; !id)
  let create_assign x a = (new_id(), I_assign (x, a))
  let create_assume b = (new_id(), I_assume b)
  let create_skip () = (new_id(), I_skip)
  let get_nodeid (id, _) = id
  let get_instr (_, instr) = instr
  let compare = Stdlib.compare
  let to_string n = 
    match n with
    | (id, I_assign (x, a)) -> 
      string_of_int id ^ ": " ^ " " ^ x ^ " := " ^ string_of_aexp a
    | (id, I_assume b) -> 
      string_of_int id ^ ": " ^ "assume"  ^ " " ^ string_of_bexp b
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
*)

let rec cmd2cfg : cmd -> Cfg.t 
= fun cmd -> let init = Cfg.empty in 
  let Seq(lst) = cmd in 
  let sp' = Node.create_skip() in let ep' = Node.create_skip() in (parsing lst init sp' ep' sp' false) 
and parsing : cmd list -> Cfg.t -> Node.t -> Node.t -> Node.t -> bool -> Cfg.t   
= fun smt cfg lp ep pr loop -> match smt with
 | hd::tl -> begin match hd with 
    | Assign (s1, a2)      -> let cur = Node.create_assign s1 a2 in Cfg.add_edge pr cur (parsing tl cfg lp ep cur loop)
    | Seq    (l1)          -> let sp' = Node.create_skip() in let ep' = Node.create_skip() in  
                              let cfg' = Cfg.add_edge pr sp' cfg in parsing l1 cfg' lp ep' sp' loop 
    | If     (b1, c2, c3)  -> cfg (*we do not have to handle this case*)
    | While  (b1, c2)      -> let sp' = Node.create_skip() in let ep' = Node.create_skip() in
                              let pass = Node.create_assume b1 in let term = Node.create_assume(Not (b1)) in  
                              let cfg = Cfg.add_edge pr sp' cfg in (* connect 4 to 5*)
                              let cfg' = Cfg.add_edge sp' pass cfg in 
                              let cfg'' = (parsing [c2] cfg' sp' ep pass true) in 
                              let cfg''' = Cfg.add_edge sp' term cfg'' in
                              let cfg'''' = (parsing tl cfg''' lp ep' term loop) in 
                              Cfg.add_edge ep' ep cfg'''' 
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

module type Interval = sig
  type atom
  type t 
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

module Interval : Interval = struct
  type atom = Const of int | N_inf | P_inf
  (* N_inf means negative infinite, P_int means postiive negative.*)
  type t = Bot | Top | Interval of atom * atom  

  let bottom = Bot 

  let string_of_atom : atom -> string
  = fun a -> match a with 
    | Const i1 -> string_of_int i1 
    | N_inf -> "N_inf"
    | P_inf -> "P_inf"

  let to_string : t -> string 
  = fun i -> match i with 
    | Bot -> "Bottom" 
    | Top -> "Top"
    | Interval(i1, i2) -> let str_i1 = string_of_atom i1 in let str_i2 = string_of_atom i2 in "[" ^ str_i1 ^ ", " ^ str_i2 ^ "]"

  (* I guess this function is for abstracting concrete values. *)
  let alpha : int -> t 
  = fun n -> Interval(Const n, Const n)  

  (* (-inf, n) *)
  let alpha_to : int -> t 
  = fun n -> Interval(N_inf, Const n)

  (* (n, inf) *)
  let alpha_from : int -> t
  = fun n -> Interval(Const n, P_inf)

  (* order for atom. only if something located in right is greater than left one, then return value would be true*)
  let comp : atom -> atom -> bool 
  = fun a b -> match a, b with 
    | _, P_inf -> true
    | P_inf, _ -> false 
    | N_inf, _ -> true 
    | _, N_inf -> false 
    | Const x, Const y -> if x <= y then true else false 

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
    | Interval(a1, a2), Interval(b1, b2) -> if (comp b1 a1) && (comp a2 b2) then true else false 

  (* okay *)
  let join a b = match a, b with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, _ -> Top 
    | _, Top -> Top 
     (* interval cases. thinks about inf cases frist!*)
    | Interval(a1, a2), Interval(b1, b2) -> if (comp a1 b1) && (comp a2 b2) then Interval(a1, b2) 
                                            else if (comp a1 b1) && (comp b2 a2) then Interval(a1, a2)
                                            else if (comp b1 a1) && (comp a2 b2) then Interval(b1, b2)
                                            else if (comp b1 a1) && (comp b2 a2) then Interval(b1, a2)
                                            else raise (Failure "Error: Impossible case in join")
   
  (* okay *)
  let meet a b = match (a, b) with  
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, y -> y 
    | x, Top -> x 
    | Interval(a1, a2), Interval(b1, b2) -> if (comp a1 b1) && (comp a2 b2) then if comp b1 a2 then Interval(b1, a2) else Bot (* intersection case *)
                                            else if (comp a1 b1) && (comp b2 a2) then Interval(b1, b2) (* this case is in situation where b is included in a.*)
                                            else if (comp b1 a1) && (comp a2 b2) then Interval(a1, a2) (* this case is in situation where a is included in b.*)
                                            else if (comp b1 a1) && (comp b2 a2) then if comp a1 b2 then Interval(a1, b2) else Bot (* intersection case *)
                                            else raise (Failure "Error: Impossible case in meet")
 
  (* this will be executed on x (widen) f(x). *)
  let widen a b = match (a, b) with  
    | Bot, y -> y
    | x, Bot -> x
    | Top, _ -> Top (* need to know more *) 
    | _, Top -> Top (* need to know more *)
    | Interval(a1, a2), Interval(b1, b2) -> let new_a = if (comp b1 a1) then N_inf else a1 in 
                                            let new_b = if (comp a2 b2) then P_inf else b1 in 
                                            Interval(new_a, new_b)
 
  let narrow a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, y -> y (* need to know more *) 
    | x, Top -> x (* need to know more *)
    | Interval(a1, a2), Interval(b1, b2) -> let new_a = if (a1 = N_inf) then b1 else a1 in 
                                            let new_b = if (a2 = P_inf) then b2 else a2 in 
                                            Interval(new_a, new_b)
 
  (* okay *)
  let add a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, _ -> Top 
    | _, Top -> Top 
    | Interval(a1, a2), Interval(b1, b2) -> 
      let new_a = (match a1, b1 with 
        | P_inf, _ -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | _, P_inf -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | N_inf, _ -> N_inf 
        | _, N_inf -> N_inf 
        | Const a1', Const b1' -> Const (a1' + b1')) in 
      let new_b = (match a2, b2 with 
        | N_inf, _ -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *) 
        | _, N_inf -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *)
        | P_inf, _ -> P_inf
        | _, P_inf -> P_inf
        | Const a2', Const b2' -> Const (a2' + b2')) in 
        Interval(new_a, new_b) 

  (* okay *)
  let sub a b = match (a, b) with 
    | Bot, _ -> Bot 
    | _, Bot -> Bot  
    | Top, _ -> Top 
    | _, Top -> Top 
    | Interval(a1, a2), Interval(b1, b2) -> 
      let new_a = (match a1, b2 with (* minimum case: a1 - b2 *) 
        | P_inf, _ -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | _, N_inf -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *)
        | N_inf, _ -> N_inf 
        | _, P_inf -> N_inf 
        | Const a1', Const b1' -> Const (a1' - b1')) in 
      let new_b = (match a2, b1 with (* maximum case: a2 - b1 *) 
        | N_inf, _ -> raise (Failure "N_inf is located in right of range.") (* this case is impossible because of widening definition *) 
        | _, P_inf -> raise (Failure "P_inf is located in left of range.") (* this case is impossible because of widening definition *)
        | P_inf, _ -> P_inf
        | _, N_inf -> P_inf
        | Const a2', Const b2' -> Const (a2' - b2')) in 
        Interval(new_a, new_b) 

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
    | Interval(Const 0, Const 0), _ -> Interval(Const 0, Const 0)
    | _, Interval(Const 0, Const 0) -> Interval(Const 0, Const 0)
    | Top, _ -> Top 
    | _, Top -> Top 
    | Interval(a1, a2), Interval(b1, b2) -> let t1 = mul_aux a1 b1 in let t2 = mul_aux a1 b2 in let t3 = mul_aux a2 b1 in let t4 = mul_aux a2 b2 in 
                                            let candidate = [t1;t2;t3;t4] in 
                                            let c1 = List.fold_right interval_min candidate t1 in 
                                            let c2 = List.fold_right interval_max candidate t1 in
                                            Interval(c1, c2)
  and mul_aux : atom -> atom -> atom 
  = fun a b -> match a, b with 
    | P_inf, z' | z', P_inf -> (match z' with
                                | P_inf -> P_inf
                                | N_inf -> N_inf
                                | Const 0 -> Const 0 
                                | Const n -> if 0 < n then P_inf else N_inf)
    | N_inf, z' | z', N_inf ->  (match z' with
                                | P_inf -> N_inf
                                | N_inf -> P_inf
                                | Const 0 -> Const 0 
                                | Const n -> if 0 < n then N_inf else P_inf)
    | Const a', Const b' -> Const (a' * b') 

  (* auxilary function for checking whether there is an intersection or not. *)
  (* Need to consider 2 inclusion cases and 2 intersection cases. *)
  let rec intersection : t -> t -> bool
  = fun a b -> match a, b with 
    | Interval(a1, a2), Interval(b1, b2) -> 
      if comp a1 b1 then (if comp b2 a2 then true else (if comp b1 a2 then true else false))
      else if comp b1 a1 then (if comp a2 b2 then true else (if comp a1 b2 then true else false))
      else false 
    | _ -> false

  (* 
     True  -> Only if a1 = a2 = a3 = a4 in const. 
     False -> case for no intersection
     Top   -> otherwise. it means there is an intersection.
  *)
  let equal a b = match (a, b) with
    | Bot, _ -> AbsBool.Bot 
    | _, Bot -> AbsBool.Bot  
    | Top, _ -> AbsBool.Top 
    | _, Top -> AbsBool.Top 
    | Interval(Const a1, Const a2), Interval(Const b1, Const b2) -> 
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
    | Interval(a1, a2), Interval(b1, b2) -> 
      if comp a2 b1 then AbsBool.True else (* this one includes a2 == b1 case.*)
        if intersection a b then AbsBool.Top else AbsBool.False  (* b1 is less than a2 without intersection. it means whole range of b is less then range of a. *)

  let ge a b = match (a,b) with   
    | Bot, _ -> AbsBool.Bot
    | _, Bot -> AbsBool.Bot
    | Top, _ -> AbsBool.Top
    | _, Top -> AbsBool.Top
    | Interval(a1, a2), Interval(b1, b2) -> 
      if comp b2 a1 then AbsBool.True else (* this one includes a2 == b1 case.*)
        if intersection a b then AbsBool.Top else AbsBool.False  (* b1 is less than a2 without intersection. it means whole range of b is less then range of a. *)
end

module VarMap = Map.Make(String)
module type AbsMem = sig
  type t = Interval.t VarMap.t 
  val empty : t
  val add : string -> Interval.t -> t -> t
  val find : string -> t -> Interval.t 
  val join : t -> t -> t 
  val widen : t -> t -> t 
  val narrow : t -> t -> t
  val order : t -> t -> bool 
  val print : t -> unit 
end

(* this one is for caculation while executing commands with absmem *)
module AbsMem : AbsMem = struct
  type t = Interval.t VarMap.t 

  let empty = VarMap.empty

  let add x v m = 
    let abs = find x m in Interval.add abs (Interval.alpha v) 

  let find x m = try VarMap.find x m with _ -> Interval.bottom 

  let rec join m1 m2 = 
    let m1' = VarMap.bindings m1 in 
    let m2  = VarMap.bindings m2 in 
    (* To-Do *)
    
  and join_aux : Interval.t -> Interval.t -> Interval.t 
  = fun a b -> Interval.join a b 
    
  let widen m1 m2 = m1 (* TODO *) 
  let narrow m1 m2 = m1 (* TODO *)
  let order m1 m2 = true (* TODO *) 

  let print m = VarMap.iter (fun x v -> prerr_endline 
    (x ^ " |-> " ^ Interval.to_string v)) m 
end


module type Table = sig
  type t = AbsMem.t NodeMap.t
  val empty : t
  val add : Node.t -> AbsMem.t -> t -> t
  val init : Node.t list -> t 
  val find : Node.t -> t -> AbsMem.t 
  val print : t -> unit
end 

module Table : Table = struct 
  type t = AbsMem.t NodeMap.t
  let empty = NodeMap.empty 
  let add = NodeMap.add
  let init ns = List.fold_right (fun n -> add n AbsMem.empty) ns empty
  let find : Node.t -> t -> AbsMem.t 
  =fun n t -> try NodeMap.find n t with _ -> AbsMem.empty
  let print t = NodeMap.iter (fun n m -> 
    prerr_endline (string_of_int (Node.get_nodeid n)); 
    AbsMem.print m; 
    prerr_endline "") t  
end

let analyze : Cfg.t -> Table.t
=fun g -> Table.empty (* TODO *)


let pgm = 
  Seq [
    Assign ("x", Const 0); 
    Assign ("y", Const 0);
    While (Le (Var "x", Const 9), 
      Seq [
        Assign ("x", Plus (Var "x", Const 1)); 
        Assign ("y", Plus (Var "y", Const 1)); 
      ]);
  ]

let cfg = cmd2cfg pgm 
let _ = Cfg.print cfg
let _ = Cfg.dot cfg
(* let table = analyze cfg 
let _ = Table.print table *)
