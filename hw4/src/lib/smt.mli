(* Helper Module for Z3 SMT Solver *)

exception Error of string

(******************************************************************************)
(******************************************************************************)
(* Constant Value                                                             *)
(******************************************************************************)
(******************************************************************************)

module CONST : sig
  (* String - Name prefix for dummy variable *)
  val _name_dummy : string
end


(******************************************************************************)
(******************************************************************************)
(* Context                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Ctx : sig
  type body = (string * string)
  type t = Z3.context
  type t_ref = t option Stdlib.ref

  val _t_obj : t_ref

  val body_timeout : unit -> body
  val create : unit -> unit
  val read : unit -> t
end


(******************************************************************************)
(******************************************************************************)
(* Symbol                                                                     *)
(******************************************************************************)
(******************************************************************************)


module Symbol : sig
  type t = Z3.Symbol.symbol

  val _name_dummy : string
  val _count_dummy : int Stdlib.ref

  val create : string -> t
  val create_dummy : unit -> t
  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Sort                                                                       *)
(******************************************************************************)
(******************************************************************************)

module Sort : sig
  type t = Z3.Sort.sort

  val create_dummy : unit -> t
  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)
(******************************************************************************)

module Expr : sig
  type t = Z3.Expr.expr

  val sort_of_void : unit -> Sort.t
  val sort_of_int : unit -> Sort.t
  val sort_of_bool : unit -> Sort.t
  val sort_of_arr : Sort.t -> Sort.t

  val of_int : int -> t
  val of_bool : bool -> t

  val zero_ : unit -> t
  val one_ : unit -> t
  val true_ : unit -> t
  val false_ : unit -> t

  val read_sort : t -> Sort.t

  val create_var : Sort.t -> name:string -> t
  val create_dummy : Sort.t -> t

  val create_arr : Sort.t -> t
  val read_arr : t -> idx:t -> t
  val update_arr : t -> idx:t -> value:t -> t

  val create_add : t -> t -> t
  val create_sub : t -> t -> t
  val create_mul : t -> t -> t
  val create_div : t -> t -> t
  val create_neg : t -> t
  val create_not : t -> t
  val create_eq : t -> t -> t
  val create_neq : t -> t -> t
  val create_lt : t -> t -> t
  val create_gt : t -> t -> t
  val create_le : t -> t -> t
  val create_ge : t -> t -> t
  val create_ite : t -> t:t -> f:t -> t
  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Fmla : sig
  type t = Expr.t

  val sort : unit -> Sort.t
  
  val true_ : unit -> t
  val false_ : unit -> t

  val uninterpreted_ : unit -> t
  val create_exp : Expr.t -> t
  val create_not : t -> t
  val create_and : t list -> t
  val create_or : t list -> t
  val create_imply : t -> t -> t
  val create_iff : t -> t -> t
  val create_forall : Expr.t -> t -> t
  val create_exists : Expr.t -> t -> t
  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Model                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module Model : sig
  type t = Z3.Model.model

  val eval : Expr.t -> model:t -> Expr.t option
  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

module Solver : sig
  type t = Z3.Solver.solver
  type validity = VAL | INVAL | UNKNOWN
  type satisfiability = SAT | UNSAT | UNKNOWN

  val _create : unit -> t
  val _formula_add : t -> Fmla.t list -> unit

  val check_satisfiability : Fmla.t list -> (satisfiability * Model.t option)
  val check_validity : Fmla.t list -> (validity * Model.t option)

  val is_unknown_sat : satisfiability -> bool
  val is_sat : satisfiability -> bool
  val is_unsat : satisfiability -> bool
  val is_unknown_val : validity -> bool
  val is_valid : validity -> bool
  val is_invalid : validity -> bool

  val to_string : t -> string
  val string_of_satisfiability : satisfiability -> string
  val string_of_validity : validity -> string
end