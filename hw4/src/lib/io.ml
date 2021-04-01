(* Input and Output Frontend Definition for HW4 *)

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
    if args.printArg then Log.info (fun m -> m "%s" (Args.to_string ())) else ();
    Log.info (fun m -> m "Read File from %s." args.inputFile);
    let in_c = Stdlib.open_in args.inputFile in
    let lexbuf = Lexing.from_channel in_c in
    try
      Log.info (fun m -> m "Start Parsing File.");
      let res = Parser.start Lexer.next_token lexbuf in
      Log.info (fun m -> m "Parsing File is Done.");
      if args.printAdt then Log.info (fun m -> m "Input Program.\n%s" (res |> Adt.string_of_pgm ~indent:1)) else ();
      close_in in_c; Timer.create (args.totalTimeout); res
    with
    | Lexer.LexingError msg -> (
      close_in in_c;
      Error ("read : " ^ msg ^ "[" ^ (position lexbuf) ^ "]") |> Stdlib.raise)
    | Parser.Error -> (
      close_in in_c;
      Error ("read : syntax error [" ^ (position lexbuf) ^ "]") |> Stdlib.raise)
    | e -> e |> Stdlib.raise
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

  let string_of_query : key:query_key -> data:query_res -> string list 
                        -> string list
  = let open Adt in
    let open Utils in
    let module CString = Core.String in
    let get_buffers : Ty.t -> string
    = fun t -> begin
      (* get_buffers function start *)
      let rec inner_get_buffers : acc:string list -> Ty.ty -> string list
      = fun ~acc t -> begin
        (* inner_get_buffers function start *)
        match t with
        | T_arr (t', b) ->
          (b |> (function Some bb -> (bb |> string_of_int) | None -> "∞"))
          ::(inner_get_buffers ~acc t')
        | _ -> acc
        (* inner_get_buffers function end *)
      end in
      inner_get_buffers t.d ~acc:[]
      |> CString.concat ~sep:" * "
      (* get_buffers function start *)
    end in
    fun ~key ~data acc -> begin
    (* string_of_query function start *)
    match key.stmt.d, key.array.d.typ.d with
    | Stmt.S_assign ((e), _), Ty.T_arr _
      when (e |> function V_arr _ -> true | _ -> false) ->
      let lin : string =
        (match key.stmt.pos with
        | Unknown     -> "?"
        | Pos (s, _)  -> s.lin |> string_of_int) in
      let buf : string = key.array.d.typ |> get_buffers in
      let validity : Smt.Solver.validity = 
        data.validity 
        |> Stdlib.fst in
      if validity = INVAL
      then
        let i : Smt.Expr.t = data.index_expr in
        let m : Smt.Model.t =
          data.validity
          |> Stdlib.snd
          |> function
              | Some mm -> mm
              | None -> (Error "read : wrong model" |> Stdlib.raise) in
        let ivalue : string = 
          i
          |> Smt.Model.eval ~model:m 
          |> function
              | Some ee -> ee |> Smt.Expr.to_string
              | None -> (Error "read : wrong index" |> Stdlib.raise) in
        ("at line " ^ lin ^ ", " ^
        (key.stmt |> string_of_stmt) ^ " " ^
        "buffer size: " ^ buf ^ ", " ^
        "index: " ^ ivalue)::acc
      else acc
    | _, _ ->
      Log.err (fun m -> m "Wrong query key (%s, %s)"
                        (key.stmt |> string_of_stmt)
                        (key.array |> string_of_decl));
      Error "read : wrong query key" |> Stdlib.raise
    (* string_of_query function end *)
  end

  let read : t -> unit
  = let open Utils in
    let module CList = Core.List in
    let module CString = Core.String in
    fun result -> begin
    (* read function start *)
    Log.info (fun m -> m "Read the Result.");
    let errs : string list =
      Map.fold
        result
        ~init:[]
        ~f:string_of_query
      |> CList.mapi
          ~f:(fun i err -> (i + 1 |> string_of_int) ^ ". " ^ err) in
    let msg = (match (CList.length errs) with
              | 0 -> "No buffer-overflow error has been detected."
              | _ -> "Buffer-overflow errors have been detected:\n" ^ 
                     (errs |> CString.concat ~sep:"\n")) in
    Log.app (fun m -> m "%s" msg)
    (* read function end *)
  end
end