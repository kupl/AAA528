(* Main Entrance Function of HW4 *)

let main : unit -> unit
= let open Lib in
  let open Utils in
  fun () -> begin
  (* main function start *)
  (* 1. Read Input File *)
  let c = Io.Input.read () in
  (* 2. Run Verifier*)
  let v = Hw4.verify c in
  (* 3. Print Result *)
  Io.Output.read v;
  Log.info (fun m -> m "Done.");
  ()
  (* main function end *)
end

let _ = begin
  Utils.Args.create ();
  Utils.Log.create ();
  Printexc.record_backtrace true;
  try
    main ()
  with
  | exc -> Utils.Log.err (fun m -> m "%s" (exc |> Printexc.to_string))
end