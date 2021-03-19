(* Abstract Data Type for Simple C Program *)

(******************************************************************************)
(******************************************************************************)
(* Common Definition                                                          *)
(******************************************************************************)
(******************************************************************************)

type id = string


(******************************************************************************)
(******************************************************************************)
(* Type                                                                       *)
(******************************************************************************)
(******************************************************************************)

module Ty = struct
  type t = 
    | T_int
    | T_bool
    | T_arr   of t

  let rec to_string : t -> string
  = let ts = to_string in (* syntax sugar *)
    fun t -> begin
    (* to_string function start *)
    match t with
    | T_int     -> "Int"
    | T_bool    -> "Bool"
    | T_arr t1  -> (t1 |> ts) ^ "[]"
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)
(******************************************************************************)

module Expr = struct
  type t =
    | E_int   of int
    | E_bool  of bool
    | E_var   of var 
    | E_add   of t * t
    | E_sub   of t * t
    | E_mul   of t * t
    | E_div   of t * t
    | E_neg   of t
    | E_len   of var
    | E_not   of t
    | E_eq    of t * t
    | E_neq   of t * t
    | E_lt    of t * t
    | E_gt    of t * t
    | E_le    of t * t
    | E_ge    of t * t
    | E_call  of id * t list

  and var =
    | V_var   of id
    | V_arr   of var * t

  let rec to_string : t -> string
  = let open Core in
    let ts = to_string in (* syntax sugar *)
    let sol = string_of_lv in (* syntax sugar *)
    fun e -> begin
    (* to_string function start *)
    match e with
    | E_int   n1          -> (n1 |> string_of_int)
    | E_bool  b1          -> (if b1 then "true" else "false")
    | E_var   v1          -> (v1 |> sol)
    | E_add   (e1, e2)    -> "(" ^ (e1 |> ts) ^ " + " ^ (e2 |> ts) ^ ")"
    | E_sub   (e1, e2)    -> "(" ^ (e1 |> ts) ^ " - " ^ (e2 |> ts) ^ ")"
    | E_mul   (e1, e2)    -> "(" ^ (e1 |> ts) ^ " * " ^ (e2 |> ts) ^ ")"
    | E_div   (e1, e2)    -> "(" ^ (e1 |> ts) ^ " / " ^ (e2 |> ts) ^ ")"
    | E_neg   e1          -> "-" ^ (e1 |> ts)
    | E_len   v1          -> "|" ^ (v1 |> sol) ^ "|"
    | E_not   e1          -> "!" ^ (e1 |> ts)
    | E_eq    (e1, e2)    -> "(" ^ (e1 |> ts) ^ " == " ^ (e2 |> ts) ^ ")"
    | E_neq   (e1, e2)    -> "(" ^ (e1 |> ts) ^ " != " ^ (e2 |> ts) ^ ")"
    | E_lt    (e1, e2)    -> "(" ^ (e1 |> ts) ^ " < "  ^ (e2 |> ts) ^ ")"
    | E_gt    (e1, e2)    -> "(" ^ (e1 |> ts) ^ " > "  ^ (e2 |> ts) ^ ")"
    | E_le    (e1, e2)    -> "(" ^ (e1 |> ts) ^ " <= " ^ (e2 |> ts) ^ ")"
    | E_ge    (e1, e2)    -> "(" ^ (e1 |> ts) ^ " >= " ^ (e2 |> ts) ^ ")"
    | E_call  (id1, el2)  -> id1 ^ "(" ^ (el2 |> List.map ~f:ts |> String.concat ~sep:", ") ^ ")"
    (* to_string function end *)
  end

  and string_of_lv : var -> string
  = let ts = to_string in (* syntax sugar *)
    let sol = string_of_lv in (* syntax sugar *)
    fun v -> begin
    (* string_of_lv function start *)
    match v with
    | V_var id1       -> id1
    | V_arr (v1, e2)  -> (v1 |> sol) ^ "[" ^ (e2 |> ts) ^ "]"
    (* string_of_lv function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Fmla = struct
  type t =
    | F_exp     of Expr.t
    | F_not     of t
    | F_and     of t list
    | F_or      of t list
    | F_imply   of t * t
    | F_iff     of t * t
    | F_forall  of Expr.var * t
    | F_exists  of Expr.var * t
  
  let create_sorted : (Expr.var * Expr.t * Expr.t) -> t
  = let open Expr in
    fun (v1, e2, e3) -> begin
    (* create_sorted function start *)
    let vi = V_var "_i" in
    let vj = V_var "_j" in
    F_forall (vi,
              F_forall (vj, 
                        F_imply (F_and [F_exp (E_le (e2, E_var vi));
                                        F_exp (E_le (E_var vi, E_var vj));
                                        F_exp (E_le (E_var vj, e3));],
                                 F_exp (E_le (E_var (V_arr (v1, E_var vi)),
                                              E_var (V_arr (v1, E_var vj)))))))
    (* create_sorted function end *)
  end

  let create_partitioned : (Expr.var * Expr.t * Expr.t * Expr.t * Expr.t) -> t
  = let open Expr in
    fun (v1, e2, e3, e4, e5) -> begin
    (* create_partitioned function start *)
    let vi = V_var "_i" in
    let vj = V_var "_j" in
    F_forall (vi,
              F_forall (vj,
                        F_imply (F_and [F_exp (E_le (e2, E_var vi));
                                        F_exp (E_le (E_var vi, e3));
                                        F_exp (E_lt (e3, e4));
                                        F_exp (E_le (e4, E_var vj));
                                        F_exp (E_le (E_var vj, e5));],
                                 F_exp (E_le (E_var (V_arr (v1, E_var vi)),
                                              E_var (V_arr (v1, E_var vj)))))))
    (* create_partitioned function end *)
  end

  let rec flatten : t -> t
  = let open Core in
    let flt = flatten in (* syntax sugar *)
    fun f -> begin
    (* flatten function start *)
    match f with
    | F_not     f1        -> F_not    (f1 |> flt)
    | F_and     fl1       -> F_and    (fl1 |> List.fold_left 
                                                ~init:[]
                                                ~f:(fun acc f1 -> f1 |> flt |> function F_and fl2 -> acc@fl2 | f2 -> acc@[f2]))
    | F_or      fl1       -> F_or     (fl1 |> List.fold_left 
                                                ~init:[]
                                                ~f:(fun acc f1 -> f1 |> flt |> function F_or fl2 -> acc@fl2 | f2 -> acc@[f2]))
    | F_imply   (f1, f2)  -> F_imply  (f1 |> flt, f2 |> flt)
    | F_iff     (f1, f2)  -> F_iff    (f1 |> flt, f2 |> flt)
    | F_forall  (v1, f2)  -> F_forall (v1, f2 |> flt)
    | F_exists  (v1, f2)  -> F_exists (v1, f2 |> flt)
    | _                   -> f
    (* flatten function end *)
  end

  let rec to_string : t -> string
  = let open Core in
    let ts = to_string in (* syntax sugar *)
    fun f -> begin
    (* to_string function start *)
    match f with
    | F_exp     e1        -> (e1 |> Expr.to_string)
    | F_not     f1        -> "~(" ^ (f1 |> ts) ^ ")"
    | F_and     fl1       -> "(" ^ (fl1 |> List.map ~f:ts |> String.concat ~sep:" && ") ^ ")"
    | F_or      fl1       -> "(" ^ (fl1 |> List.map ~f:ts |> String.concat ~sep:" || ") ^ ")"
    | F_imply   (f1, f2)  -> "(" ^ (f1 |> ts) ^ " -> "  ^ (f2 |> ts) ^ ")"
    | F_iff     (f1, f2)  -> "(" ^ (f1 |> ts) ^ " <-> " ^ (f2 |> ts) ^ ")"
    | F_forall  (v1, f2)  -> "(forall (" ^ (v1 |> Expr.string_of_lv) ^ "). " ^ (f2 |> ts) ^ ")"
    | F_exists  (v1, f2)  -> "(exists (" ^ (v1 |> Expr.string_of_lv) ^ "). " ^ (f2 |> ts) ^ ")"
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Invariant                                                                  *)
(******************************************************************************)
(******************************************************************************)

module Inv = struct
  type t = {
    id: id;
    f: Fmla.t;
  }

  let create : Fmla.t -> id:id -> t
  = fun f1 ~id -> begin
    (* create function start *)
    { id=id;
      f=f1; }
    (* create function end *)
  end

  let to_string : t -> string
  = fun i -> begin
    (* to_string function start *)
    "@" ^ i.id ^ ":\t" ^ (i.f |> Fmla.to_string)
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Ranking Function                                                           *)
(******************************************************************************)
(******************************************************************************)

module Rank = struct
  type t = Expr.t list

  let to_string : t -> string
  = let open Core in
    fun r -> begin
    (* to_string function start *)
    "#\t(" ^ (r |> List.map ~f:Expr.to_string |> String.concat ~sep:", ") ^ ")"
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Statement                                                                  *)
(******************************************************************************)
(******************************************************************************)

module Stmt = struct
  type t =
    | S_seq     of t list
    | S_skip
    | S_assign  of Expr.var * Expr.t
    | S_if      of Expr.t * t * t
    | S_while   of Inv.t * Rank.t option * Expr.t * t
    | S_return  of Expr.t
    | S_break

  let rec flatten : t -> t
  = let open Core in
    let flt = flatten in (* syntax sugar *)
    fun s -> begin
    (* flatten function start *)
    match s with
    | S_seq     sl1               -> S_seq    (sl1 |> List.fold_left
                                                        ~init:[]
                                                        ~f:(fun acc s1 -> s1 |> flt |> function S_seq sl2 -> acc@sl2 | s2 -> acc@[s2]))
    | S_if      (e1, s2, s3)      -> S_if     (e1, s2 |> flt, s3 |> flt)
    | S_while   (i1, r2, e3, s4)  -> S_while  (i1, r2, e3, s4 |> flt)
    | _                           -> s
    (* flatten function end *)
  end

  let to_string : ?indent:int -> t -> string
  = let open Core in
    fun ?(indent=0) s -> begin
    (* to_string function start *)
    let rec inner_to_string : ?depth:int -> t -> string (* inner function of to_string *)
    = fun ?(depth=indent+1) s -> begin
      (* inner_to_string function start *)
      let its = inner_to_string ~depth in (* syntax sugar *)
      let its_deep = inner_to_string ~depth:(depth + 1) in (* syntax sugar *)
      let tb = char_of_int 9 in (* \t *)
      let idt = String.make depth tb in (* \t * depth *)
      match s with
      | S_seq     sl1               -> sl1 |> List.map ~f:its |> String.concat ~sep:"\n"
      | S_skip                      -> idt ^ "skip;"
      | S_assign  (v1, e2)          -> idt ^ (v1 |> Expr.string_of_lv) ^ " := " ^ (e2 |> Expr.to_string) ^ ";"
      | S_if      (e1, s2, s3)      -> idt ^ "if (" ^ (e1 |> Expr.to_string) ^ ") {\n" ^
                                         (s2 |> its_deep) ^ "\n" ^
                                       idt ^ "} {\n" ^
                                         (s3 |> its_deep) ^ "\n" ^
                                       idt ^ "}"
      | S_while   (i1, r2, e3, s4)  -> idt ^ "while\n" ^
                                       idt ^ "\t" ^ (i1 |> Inv.to_string) ^ "\n" ^
                                       (if (r2 |> Option.is_some) then (idt ^ "\t" ^ (r2 |> Option.value ~default:[] |> Rank.to_string) ^ "\n") else "") ^
                                       idt ^ "\t(" ^ (e3 |> Expr.to_string) ^ ")\n" ^
                                       idt ^ "{\n" ^
                                         (s4 |> its_deep) ^ "\n" ^
                                       idt ^ "}"
      | S_return  e1                -> idt ^ "return " ^ (e1 |> Expr.to_string) ^ ";"
      | S_break                     -> idt ^ "break;"
      (* inner_to_string function end *)
    end in
    inner_to_string s
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Declaration of a Variable                                                  *)
(******************************************************************************)
(******************************************************************************)

module Decl = struct
  type t = {
    typ: Ty.t;
    id: id;
  }

  let create : Ty.t -> id:id -> t
  = fun t1 ~id -> begin
    (* create function start *)
    { typ=t1;
      id=id; }
    (* create function end *)
  end

  let to_string : t -> string
  = fun { typ; id } -> begin
    (* to_string function start *)
    (typ |> Ty.to_string) ^ " " ^ id
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Program                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Pgm = struct
  type t = {
    pre: Inv.t;
    post: Inv.t;
    rank: Rank.t option;
    typ: Ty.t;
    id: id;
    args: Decl.t list;
    locals: Decl.t list;
    stmt: Stmt.t;
  }

  let create : Stmt.t 
               -> pre:Inv.t 
               -> post:Inv.t 
               -> rank:Rank.t option 
               -> typ:Ty.t 
               -> id:id 
               -> args:Decl.t list 
               -> locals:Decl.t list 
               -> t
  = fun s1 ~pre ~post ~rank ~typ ~id ~args ~locals -> begin
    (* create function start *)
    { pre=pre;
      post=post;
      rank=rank;
      typ=typ;
      id=id;
      args=args;
      locals=locals;
      stmt=s1; }
    (* create function end *)
  end
  
  let to_string : ?indent:int -> t -> string
  = let open Core in
    fun ?(indent=0) { pre; post; rank; typ; id; args; locals; stmt } -> begin
    (* to_string function start *)
    let tb = char_of_int 9 in (* \t *)
    let idt = String.make indent tb in (* \t * depth *)
    idt ^ (pre |> Inv.to_string) ^ "\n" ^
    idt ^ (post |> Inv.to_string) ^ "\n" ^
    idt ^ (if (rank |> Option.is_some) then ((rank |> Option.value ~default:[] |> Rank.to_string) ^ "\n") else "") ^
    idt ^ (typ |> Ty.to_string) ^ " " ^ id ^ " (" ^ (args |> List.map ~f:Decl.to_string |> String.concat ~sep:", ") ^ ") {\n" ^
    idt ^ "\t// locals: (" ^ (locals |> List.map ~f:Decl.to_string |> String.concat ~sep:", ") ^ ")\n" ^
    (stmt |> Stmt.to_string ~indent:(indent+1)) ^ "\n" ^
    idt ^ "}"
    (* to_String function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Alias                                                                      *)
(******************************************************************************)
(******************************************************************************)

type t = Pgm.t

let string_of_id : id -> string
= fun id1 -> id1

let string_of_typ : Ty.t -> string
= Ty.to_string

let string_of_expr : Expr.t -> string
= Expr.to_string

let string_of_lv : Expr.var -> string
= Expr.string_of_lv

let string_of_fmla : Fmla.t -> string
= Fmla.to_string

let string_of_inv : Inv.t -> string
= Inv.to_string

let string_of_rank : Rank.t -> string
= Rank.to_string

let string_of_stmt : ?indent:int -> Stmt.t -> string
= Stmt.to_string

let string_of_decl : Decl.t -> string
= Decl.to_string

let string_of_pgm : ?indent:int -> Pgm.t -> string
= Pgm.to_string
