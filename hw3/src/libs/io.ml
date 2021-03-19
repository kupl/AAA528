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
  = fun () -> begin
    (* read function start *)
    let args = Utils.Args.read () in
    let in_c = Stdlib.open_in args.inputFile in
    let lexbuf = Lexing.from_channel in_c in
    try
      let res = Parser.start Lexer.next_token lexbuf in
      close_in in_c; res
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
