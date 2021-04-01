(* Input and Output Frontend Definition for HW4 *)

(******************************************************************************)
(******************************************************************************)
(* Input                                                                      *)
(******************************************************************************)
(******************************************************************************)

module Input : sig
  type t = Adt.Pgm.t

  exception Error of string

  val position : Lexing.lexbuf -> string
  val read : unit -> t
  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Output                                                                     *)
(******************************************************************************)
(******************************************************************************)

module Output : sig
  type  t = (query_key, query_res) Map.t
  and   query_key = { (* identifying information of each query *)
    stmt:       Adt.Stmt.t; (* must be S_assign ((V_arr _), _) *)
    array:      Adt.Decl.t; (* must be { typ: T_arr _; id: _ } *)
  }
  and   query_res = { (* verification result of each query *)
    index_expr: Smt.Expr.t;
    validity:   Smt.Solver.validity * Smt.Model.t option;
  }

  exception Error of string

  val read : t -> unit
end