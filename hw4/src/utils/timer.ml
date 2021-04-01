(* Program Timer for HW4 *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct

  let timeout_handler : int -> unit
  = fun _ -> begin
    (* handler function start *)
    Log.err (fun m -> m "Timebudget is Over. Halt the Program.");
    exit 1;
    (* handler function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let create : int -> unit
= fun sec -> begin
  (* create function start *)
  Log.info (fun m -> m "Start the Timer for the Program.");
  let _ = Stdlib.ignore (Unix.alarm sec) in
  ()
  (* create function end *)
end

let _ = Sys.set_signal Sys.sigalrm (Sys.Signal_handle (Setting.timeout_handler))