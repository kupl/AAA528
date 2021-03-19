(* Main Entrance Function of HW3 *)

let main : unit -> unit
= fun () -> begin
  (* main function start *)
  (* 1. Read Input File *)
  let c = Libs.Io.Input.read () in
  (* 2. Run Verifier*)
  let v = Hw3.verify c in
  (* 3. Print Result *)
  Libs.Io.Output.read v
  (* main function end *)
end

let _ = begin
  Utils.Args.create ();
  Printexc.record_backtrace true;
  try
    main ()
  with
  | exc -> prerr_endline (Printexc.to_string exc)
end