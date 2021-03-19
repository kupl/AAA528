(* Program Argument Definition for HW3 *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  (* STRING - input file path *)
  val inputFile : string Stdlib.ref
  (* FLAG - verbose log mode *)
  val verbose : bool Stdlib.ref
  (* INT - Z3 timebudget in seconds *)
  val z3Timeout : int Stdlib.ref
  (* INT - total timebudget in seconds *)
  val totalTimeout : int Stdlib.ref
  (* FLAG - verification mode flag *)
  val partial : bool Stdlib.ref
  val total : bool Stdlib.ref

  val speclist : (Arg.key * Arg.spec * Arg.doc) list
  val anon_fun : string -> unit
  val usage_msg : string

  val validate_arg : unit -> unit
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = {
  inputFile: string;
  verbose: bool;
  z3Timeout: int;
  totalTimeout: int;
  partial: bool;
  total: bool;
}

val create : unit -> unit
val read : unit -> t
val to_string : unit -> string