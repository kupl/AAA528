(* Abstract Data Type for Simple C Program *)

(******************************************************************************)
(******************************************************************************)
(* Common Definition                                                          *)
(******************************************************************************)
(******************************************************************************)

type  id = string

type  loc = 
  | Unknown
  | Pos of pos * pos
and   pos = { 
  col:  int; 
  lin:  int; 
}

type  'a c = {
  pos:  loc;
  d:    'a;
}

let create_c : 'a -> pos:loc -> 'a c
= fun d ~pos -> begin
  (* create_c function start *)
  { pos=pos;
    d=d; }
  (* create_c function end *)
end


(******************************************************************************)
(******************************************************************************)
(* Type                                                                       *)
(******************************************************************************)
(******************************************************************************)

module Ty = struct
  type  buffer = int
  type  t = ty c
  and   ty =
    | T_void
    | T_int
    | T_bool
    | T_arr   of ty * buffer option

  let create : ty -> pos:loc -> t
  = create_c (* Alias *)

  let rec string_of_ty : ty -> string
  = let sot = string_of_ty in (* syntax sugar *)
    fun t -> begin
    (* string_of_ty function start *)
    match t with
    | T_void          -> "Void"
    | T_int           -> "Int"
    | T_bool          -> "Bool"
    | T_arr (t1, l2)  -> (t1 |> sot) ^ "["^ (if Option.is_some l2 then l2 |> Option.get |> string_of_buffer else "") ^"]"
    (* string_of_ty function end *)
  end

  and string_of_buffer : buffer -> string
  = string_of_int (* Alias *)

  let to_string : t -> string
  = fun t -> begin
    (* to_string function start *)
    string_of_ty t.d
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)
(******************************************************************************)

module Expr = struct
  type  t = exp c
  and   exp =
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
  and   var =
    | V_var   of id
    | V_arr   of var * t

  let create : exp -> pos:loc -> t
  = create_c (* Alias *)

  let rec string_of_exp : exp -> string
  = let module CList = Core.List in
    let module CString = Core.String in
    let ts = to_string in (* syntax sugar *)
    let sol = string_of_lv in (* syntax sugar *)
    fun e -> begin
    (* string_of_exp function start *)
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
    | E_call  (id1, el2)  -> id1 ^ "(" ^ (el2 |> CList.map ~f:ts |> CString.concat ~sep:", ") ^ ")"
    (* string_of_exp function end *)
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

  and to_string : t -> string
  = fun e -> begin
    (* to_string function start *)
    string_of_exp e.d
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Fmla = struct
  type  t = fmla c
  and   fmla =
    | F_exp     of Expr.t
    | F_not     of t
    | F_and     of t list
    | F_or      of t list
    | F_imply   of t * t
    | F_iff     of t * t
    | F_forall  of Expr.var * t
    | F_exists  of Expr.var * t
  
  let create : fmla -> pos:loc -> t
  = create_c (* Alias *)

  let create_sorted : (Expr.var * Expr.t * Expr.t) -> pos:loc -> t
  = fun (v1, e2, e3) ~pos -> begin
    (* create_sorted function start *)
    let ec = Expr.create ~pos in (* syntax sugar *)
    let fc = create ~pos in (* syntax sugar *)
    (* 1. create expressions *)
    let vari        = Expr.V_var "_i" in
    let varj        = Expr.V_var "_j" in
    let e_vi        = ec (E_var vari) in
    let e_vj        = ec (E_var varj) in
    let e_v1_vi     = ec (E_var (V_arr (v1, e_vi))) in
    let e_v1_vj     = ec (E_var (V_arr (v1, e_vj))) in
    let e_e2_le_vi  = ec (E_le  (e2,      e_vi)) in
    let e_vi_le_vj  = ec (E_le  (e_vi,    e_vj)) in
    let e_vj_le_e3  = ec (E_le  (e_vj,    e3)) in
    let e_v1_sorted = ec (E_le  (e_v1_vi, e_v1_vj)) in
    (* 2. create formulas *)
    let f_e2_le_vi  = fc (F_exp e_e2_le_vi) in
    let f_vi_le_vj  = fc (F_exp e_vi_le_vj) in
    let f_vj_le_e3  = fc (F_exp e_vj_le_e3) in
    let f_pre       = fc (F_and [f_e2_le_vi; f_vi_le_vj; f_vj_le_e3]) in
    let f_con       = fc (F_exp e_v1_sorted) in
    let f_imply     = fc (F_imply (f_pre, f_con)) in
    let f_forall_j  = fc (F_forall (varj, f_imply)) in
    let f_forall_i  = create (F_forall (vari, f_forall_j)) ~pos in
    f_forall_i
    (* create_sorted function end *)
  end

  let create_partitioned : (Expr.var * Expr.t * Expr.t * Expr.t * Expr.t) -> pos:loc -> t
  = fun (v1, e2, e3, e4, e5) ~pos -> begin
    (* create_partitioned function start *)
    let ec = Expr.create ~pos in (* syntax sugar *)
    let fc = create ~pos in (* syntax sugar *)
    (* 1. create expressions *)
    let vari        = Expr.V_var "_i" in
    let varj        = Expr.V_var "_j" in
    let e_vi        = ec (E_var vari) in
    let e_vj        = ec (E_var varj) in
    let e_v1_vi     = ec (E_var (V_arr (v1, e_vi))) in
    let e_v1_vj     = ec (E_var (V_arr (v1, e_vj))) in
    let e_e2_le_vi  = ec (E_le  (e2,      e_vi)) in
    let e_vi_le_e3  = ec (E_le  (e_vi,    e3)) in
    let e_e3_lt_e4  = ec (E_le  (e3,      e4)) in
    let e_e4_le_vj  = ec (E_le  (e4,      e_vj)) in
    let e_vj_le_e5  = ec (E_le  (e_vj,    e5)) in
    let e_v1_parted = ec (E_le  (e_v1_vi, e_v1_vj)) in
    (* 2. create formulas *)
    let f_e2_le_vi  = fc (F_exp e_e2_le_vi) in
    let f_vi_le_e3  = fc (F_exp e_vi_le_e3) in
    let f_e3_lt_e4  = fc (F_exp e_e3_lt_e4) in
    let f_e4_le_vj  = fc (F_exp e_e4_le_vj) in
    let f_vj_le_e5  = fc (F_exp e_vj_le_e5) in
    let f_pre       = fc (F_and [f_e2_le_vi; f_vi_le_e3; f_e3_lt_e4; f_e4_le_vj; f_vj_le_e5]) in
    let f_con       = fc (F_exp e_v1_parted) in
    let f_imply     = fc (F_imply (f_pre, f_con)) in
    let f_forall_j  = fc (F_forall (varj, f_imply)) in
    let f_forall_i  = create (F_forall (vari, f_forall_j)) ~pos in
    f_forall_i
    (* create_partitioned function end *)
  end

  let rec flatten : t -> t
  = let module CList = Core.List in
    let flt = flatten in (* syntax sugar *)
    fun f -> begin
    (* flatten function start *)
    let fc = create ~pos:f.pos in (* syntax sugar *)
    match f.d with
    | F_not     f1        -> fc (F_not    (f1 |> flt))
    | F_and     fl1       -> fc (F_and    (fl1 |> CList.fold_left 
                                                  ~init:[]
                                                  ~f:(fun acc f1 -> f1 |> flt |> fun f' -> match f'.d with F_and fl2 -> acc@fl2 | _ -> acc@[f'])))
    | F_or      fl1       -> fc (F_or     (fl1 |> CList.fold_left 
                                                  ~init:[]
                                                  ~f:(fun acc f1 -> f1 |> flt |> fun f' -> match f'.d with F_or fl2 -> acc@fl2 | _ -> acc@[f'])))
    | F_imply   (f1, f2)  -> fc (F_imply  (f1 |> flt, f2 |> flt))
    | F_iff     (f1, f2)  -> fc (F_iff    (f1 |> flt, f2 |> flt))
    | F_forall  (v1, f2)  -> fc (F_forall (v1, f2 |> flt))
    | F_exists  (v1, f2)  -> fc (F_exists (v1, f2 |> flt))
    | _                   -> f
    (* flatten function end *)
  end

  let rec string_of_fmla : fmla -> string
  = let module CList = Core.List in
    let module CString = Core.String in
    let ts = to_string in (* syntax sugar *)
    fun f -> begin
    (* to_string function start *)
    match f with
    | F_exp     e1        -> (e1 |> Expr.to_string)
    | F_not     f1        -> "~(" ^ (f1 |> ts) ^ ")"
    | F_and     fl1       -> "(" ^ (fl1 |> CList.map ~f:ts |> CString.concat ~sep:" && ") ^ ")"
    | F_or      fl1       -> "(" ^ (fl1 |> CList.map ~f:ts |> CString.concat ~sep:" || ") ^ ")"
    | F_imply   (f1, f2)  -> "(" ^ (f1 |> ts) ^ " -> "  ^ (f2 |> ts) ^ ")"
    | F_iff     (f1, f2)  -> "(" ^ (f1 |> ts) ^ " <-> " ^ (f2 |> ts) ^ ")"
    | F_forall  (v1, f2)  -> "(forall (" ^ (v1 |> Expr.string_of_lv) ^ "). " ^ (f2 |> ts) ^ ")"
    | F_exists  (v1, f2)  -> "(exists (" ^ (v1 |> Expr.string_of_lv) ^ "). " ^ (f2 |> ts) ^ ")"
    (* to_string function end *)
  end

  and to_string : t -> string
  = fun f -> begin
    (* to_string function start *)
    string_of_fmla f.d
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Invariant                                                                  *)
(******************************************************************************)
(******************************************************************************)

module Inv = struct
  type  t = inv c
  and   inv = {
    id: id;
    f: Fmla.t;
  }

  let create : Fmla.t -> id:id -> pos:loc -> t
  = fun f1 ~id ~pos -> begin
    (* create function start *)
    create_c { id=id; f=f1; } ~pos
    (* create function end *)
  end

  let to_string : t -> string
  = fun i -> begin
    (* to_string function start *)
    "@" ^ i.d.id ^ ":\t" ^ (i.d.f |> Fmla.to_string)
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Statement                                                                  *)
(******************************************************************************)
(******************************************************************************)

module Stmt = struct
  type  t = stmt c 
  and   stmt =
    | S_seq     of t list
    | S_skip
    | S_assign  of Expr.var * Expr.t
    | S_if      of Expr.t * t * t
    | S_while   of Expr.t * t
    | S_return  of Expr.t
    | S_break

  let create : stmt -> pos:loc -> t
  = create_c (* Alias *)
  let rec flatten : t -> t
  = let module CList = Core.List in
    let flt = flatten in (* syntax sugar *)
    fun s -> begin
    (* flatten function start *)
    let sc = create ~pos:s.pos in
    match s.d with
    | S_seq     sl1               -> sc (S_seq    (sl1 |> CList.fold_left
                                                          ~init:[]
                                                          ~f:(fun acc s1 -> s1 |> flt |> fun s' -> match s'.d with S_seq sl2 -> acc@sl2 | _ -> acc@[s'])))
    | S_if      (e1, s2, s3)      -> sc (S_if     (e1, s2 |> flt, s3 |> flt))
    | S_while   (e1, s2)          -> sc (S_while  (e1, s2 |> flt))
    | _                           -> s
    (* flatten function end *)
  end

  let rec string_of_stmt : ?indent:int -> stmt -> string
  = let module CList = Core.List in
    let module CString = Core.String in
    fun ?(indent=0) s -> begin
    (* string_of_stmt function start *)
    let ts = to_string ~indent in (* syntax sugar *)
    let ts_deep = to_string ~indent:(indent + 1) in (* syntax sugar *)
    let tb = char_of_int 9 in (* \t *)
    let idt = String.make indent tb in (* \t * depth *)
    match s with
    | S_seq     sl1           -> sl1 |> CList.map ~f:ts |> CString.concat ~sep:"\n"
    | S_skip                  -> idt ^ "skip;"
    | S_assign  (v1, e2)      -> idt ^ (v1 |> Expr.string_of_lv) ^ " = " ^ (e2 |> Expr.to_string) ^ ";"
    | S_if      (e1, s2, s3)  -> idt ^ "if (" ^ (e1 |> Expr.to_string) ^ ") {\n" ^
                                    (s2 |> ts_deep) ^ "\n" ^
                                 idt ^ "} {\n" ^
                                    (s3 |> ts_deep) ^ "\n" ^
                                 idt ^ "}"
    | S_while   (e1, s2)      -> idt ^ "while\n" ^
                                 idt ^ "\t(" ^ (e1 |> Expr.to_string) ^ ")\n" ^
                                 idt ^ "{\n" ^
                                    (s2 |> ts_deep) ^ "\n" ^
                                 idt ^ "}"
    | S_return  e1            -> idt ^ "return " ^ (e1 |> Expr.to_string) ^ ";"
    | S_break                 -> idt ^ "break;"
    (* string_of_stmt function end *)
  end

  and to_string : ?indent:int -> t -> string
  = let module CList = Core.List in
    let module CString = Core.String in
    fun ?(indent=0) s -> begin
    (* to_string function start *)
    string_of_stmt ~indent:indent s.d
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Declaration of a Variable                                                  *)
(******************************************************************************)
(******************************************************************************)

module Decl = struct
  type  t = decl c
  and   decl = {
    typ: Ty.t;
    id: id;
  }

  let create : Ty.t -> id:id -> pos:loc -> t
  = fun t1 ~id ~pos -> begin
    (* create function start *)
    create_c { typ=t1; id=id; } ~pos
    (* create function end *)
  end

  let to_string : t -> string
  = fun d1 -> begin
    (* to_string function start *)
    let { typ; id } = d1.d in
    (typ |> Ty.to_string) ^ " " ^ id
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Function                                                                   *)
(******************************************************************************)
(******************************************************************************)

module Func = struct
  type  t = {
    pre:    Inv.t;
    typ:    Ty.t;
    id:     id;
    args:   Decl.t list;
    locals: Decl.t list;
    stmt:   Stmt.t;
  }

  let create : Stmt.t 
               -> pre:Inv.t 
               -> typ:Ty.t 
               -> id:id 
               -> args:Decl.t list 
               -> locals:Decl.t list 
               -> t
  = fun s1 ~pre ~typ ~id ~args ~locals -> begin
    (* create function start *)
    { pre=pre; typ=typ; id=id; args=args; locals=locals; stmt=s1; }
    (* create function end *)
  end
  
  let to_string : ?indent:int -> t -> string
  = let module CList = Core.List in
    let module CString = Core.String in
    fun ?(indent=0) { pre; typ; id; args; locals; stmt } -> begin
    (* to_string function start *)
    let tb = char_of_int 9 in (* \t *)
    let idt = CString.make indent tb in (* \t * depth *)
    idt ^ (pre |> Inv.to_string) ^ "\n" ^
    idt ^ (typ |> Ty.to_string) ^ " " ^ id ^ " (" ^ (args |> CList.map ~f:Decl.to_string |> CString.concat ~sep:", ") ^ ") {\n" ^
    idt ^ "\t// locals: (" ^ (locals |> CList.map ~f:Decl.to_string |> CString.concat ~sep:", ") ^ ")\n" ^
      (stmt |> Stmt.to_string ~indent:(indent+1)) ^ "\n" ^
    idt ^ "}"
    (* to_string function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Program                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Pgm = Func


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

let string_of_stmt : ?indent:int -> Stmt.t -> string
= Stmt.to_string

let string_of_decl : Decl.t -> string
= Decl.to_string

let string_of_func : ?indent:int -> Func.t -> string
= Func.to_string

let string_of_pgm : ?indent:int -> Pgm.t -> string
= Pgm.to_string