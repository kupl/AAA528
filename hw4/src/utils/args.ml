(* Program Argument Definition for HW4 *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  (* STRING - input file path *)
  let inputFile : string Stdlib.ref
  = Stdlib.ref ""

  (* FLAG - verbose log mode *)
  let verbose : bool Stdlib.ref
  = Stdlib.ref false

  (* INT - Z3 timebudget in seconds *)
  let z3Timeout : int Stdlib.ref
  = Stdlib.ref 30

  (* INT - total timebudget in seconds *)
  let totalTimeout : int Stdlib.ref
  = Stdlib.ref 180

  (* FLAG - print data *)
  let printArg : bool Stdlib.ref
  = Stdlib.ref false
  let printAdt : bool Stdlib.ref
  = Stdlib.ref false

  let speclist : (Arg.key * Arg.spec * Arg.doc) list
  = [
    ("--input",         (Arg.String (fun s -> inputFile := s)), 
                        "File path for input simple c program");
    ("--verbose",       (Arg.Set verbose), 
                        "Verbose log mode");
    ("--z3-timeout",    (Arg.Int (fun d -> z3Timeout := d)), 
                        "Timebudget for Z3 solver - default - 30s");
    ("--total-timeout", (Arg.Int (fun d -> totalTimeout := d)), 
                        "Timebudget for program (if time is over, the program will be halt) - default: 180s");
    ("--print-arg",     (Arg.Set printArg),
                        "Print out arguments of this process");
    ("--print-adt",     (Arg.Set printAdt),
                        "Print out ADT form of input program");]
  
  let anon_fun : string -> unit
  = fun s -> begin
    (* anon_fun function start *)
    match s with
    | _     -> invalid_arg "invalid option"
    (* anon_fun function end *)
  end
  
  let usage_msg : string
  = "aaa528_hw4 [--input] file [options]"

  let validate_arg : unit -> unit
  = let file_exists : string -> bool
    = fun name -> begin
      (* file_exists function start *)
      try Unix.access name [Unix.F_OK]; true
      with
      | (Unix.Unix_error (Unix.ENOENT, _, _)) -> false
      | _ -> true
      (* file_exists function end *)
    end in
    fun () -> begin
    (* validate_arg function start *)
    if not (file_exists !inputFile) then  Stdlib.invalid_arg "invalid input" else
    if !z3Timeout > !totalTimeout then    Stdlib.invalid_arg "invalid timebudget - z3 timebudget should be less than total timebudget" else
    ()
    (* validate_arg function end *)
  end
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
  printArg: bool;
  printAdt: bool;
}

let create : unit -> unit
= let open Setting in
  fun () -> begin
  (* create function start *)
  Arg.parse speclist anon_fun usage_msg;
  validate_arg ();
  ()
  (* create function end *)
end

let read : unit -> t
= let open Setting in
  fun () -> begin
  (* read function start *)
  { inputFile=(!inputFile);
    verbose=(!verbose);
    z3Timeout=(!z3Timeout);
    totalTimeout=(!totalTimeout);
    printArg=(!printArg);
    printAdt=(!printAdt); }
  (* read function end *)
end

let to_string : unit -> string
= fun () -> begin
  let args = read () in
  "Current Arguments.\n" ^
  "\t- Input File: " ^ args.inputFile ^ "\n" ^
  "\t- Verbose: " ^ (args.verbose |> string_of_bool) ^ "\n" ^
  "\t- Z3 Timebudget: " ^ (args.z3Timeout |> string_of_int) ^ "\n" ^
  "\t- Total Timebudget: " ^ (args.totalTimeout |> string_of_int) ^ "\n" ^
  "\t- Print Arguments Flag: " ^ (args.printArg |> string_of_bool) ^ "\n" ^
  "\t- Print ADT Flag: " ^ (args.printAdt |> string_of_bool)
end