(* Main Entrance of Student Codes *)


(* TODO *)


(******************************************************************************)
(******************************************************************************)
(* Entrance of Student Code                                                   *)
(******************************************************************************)
(******************************************************************************)

let verify : Lib.Io.Input.t -> Lib.Io.Output.t
= let open Lib in
  fun c -> begin
  (* verify function start *)
  let query_map : Lib.Io.Output.t = Map.empty in
  (* TODO *)

  (********************************************)
  (*************** Example Code ***************)
  let _ = c in
  let open Adt in
  let sample_pos        : loc = Pos ({ col=1; lin=1; }, { col=2; lin=1; }) in
  let sample_buffer     : Ty.buffer = 10 in
  let sample_index      : id = "sample_i" in
  let sample_array      : id = "sample_a" in
      (*****************************************
        Sample query key:
          Statement: "sample_a[sample_i] := 1"
          Array: "int[10] sample_a"
      *****************************************)
  let sample_query_key  : Lib.Io.Output.query_key = { 
    stmt=(Stmt.S_assign (
            (Expr.V_arr (
              (Expr.V_var sample_array), 
              (Expr.E_var (Expr.V_var sample_index)
                |> Expr.create ~pos:sample_pos))), 
            Expr.create (Expr.E_int 1) ~pos:sample_pos)
          |> Stmt.create ~pos:sample_pos);
    array=(Ty.create (Ty.T_arr (Ty.T_int, Some sample_buffer)) ~pos:sample_pos
          |> Decl.create ~id:"sample" ~pos:sample_pos); } in

      (*****************************************
        Formula: sample_i < 10
        Sample query res:
          Index Expression: sample_i
          Validity: (INVAL, Some model)
      *****************************************)
  let sample_z_index    : Smt.Expr.t = Smt.Expr.create_var (Smt.Expr.sort_of_int ()) ~name:sample_index in
  let sample_query_res  : Lib.Io.Output.query_res = {
    index_expr=sample_z_index;
    validity=Smt.Solver.check_validity
              [(Smt.Expr.create_lt
                  sample_z_index
                  (sample_buffer |> Smt.Expr.of_int)
                |> Smt.Fmla.create_exp)]; } in

      (****************************************)
  let buffer_overflow_error_detected_output = false in
  let query_map = (
    if buffer_overflow_error_detected_output
    then Map.set query_map ~key:sample_query_key ~data:sample_query_res
    else Map.set query_map ~key:sample_query_key ~data:{ sample_query_res with validity = (VAL, None)} ) in
  
  (*************** Example Code ***************)
  (********************************************)

  (* TODO *)
  query_map
  (* verify function end *)
end