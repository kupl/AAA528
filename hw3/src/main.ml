(* Main Entrancy Function of HW3 *)

let main : unit -> unit
= fun () -> begin
  (* main function start *)
  ()
  (* main function end *)
end

let _ = begin
  Printexc.record_backtrace true;
  try
    main ()
  with
  | exc -> prerr_endline (Printexc.to_string exc)
end