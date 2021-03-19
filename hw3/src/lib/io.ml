(* Input and Output Frontend Definition for HW3 *)

(******************************************************************************)
(******************************************************************************)
(* Input                                                                      *)
(******************************************************************************)
(******************************************************************************)

module Input = struct
  type t = Adt.Pgm.t

  exception Error of string

  let position : Lexing.lexbuf -> string
  = fun lexbuf -> begin
    (* position function start *)
    let pos = lexbuf.lex_curr_p in
    pos.pos_fname ^ ":" ^ 
    (pos.pos_lnum |> string_of_int) ^ ":" ^ 
    ((pos.pos_cnum - pos.pos_bol + 1) |> string_of_int)
    (* position function end *)
  end

  let read : unit -> t
  = let open Utils in
    fun () -> begin
    (* read function start *)
    let args = Args.read () in
    Log.info (fun m -> m "Read File from %s." args.inputFile);
    let in_c = Stdlib.open_in args.inputFile in
    let lexbuf = Lexing.from_channel in_c in
    try
      Log.info (fun m -> m "Start Parsing File.");
      let res = Parser.start Lexer.next_token lexbuf in
      Log.info (fun m -> m "Parsing File is Done.");
      close_in in_c; Timer.create (args.totalTimeout); res
    with
    | Lexer.LexingError msg -> (
      close_in in_c;
      Error ("read: " ^ msg ^ "[" ^ (position lexbuf) ^ "]") |> Stdlib.raise)
    | Parser.Error -> (
      close_in in_c;
      Error ("read: syntax error [" ^ (position lexbuf) ^ "]") |> Stdlib.raise)
    (* read function end *)
  end

  let to_string : t -> string
  = Adt.string_of_pgm (* alias *)
end


(******************************************************************************)
(******************************************************************************)
(* Output                                                                     *)
(******************************************************************************)
(******************************************************************************)

module Output = struct
  type t = Smt.Solver.validity

  exception Error

  let read : t -> unit
  = let open Smt.Solver in
    let open Utils in
    fun result -> begin
    (* read function start *)
    let args = Args.read () in
    Log.info (fun m -> m "Read the Result.");
    let msg = (match (result, args.partial) with
              | VAL, true   -> "Proved that the program is partially correct w.r.t. the pre/post conditions."
              | VAL, false  -> "Proved that the program always terminates."
              | _           -> "Failed to verify the program.") in
    Log.app (fun m -> m "%s" msg)
    (* read function end *)
  end
end