%{
  open Adt

  let locals = Stdlib.ref []
  let inv_num = Stdlib.ref 0

  let new_inv_id : string -> id
  = let open Core in
    let default_id = "L" in
    let chop : string -> (string * int) (* syntax sugar *)
    = let is_digit : char -> bool (* syntax sugar *)
      = function '0' .. '9' -> true | _ -> false in 
      fun id -> begin
      (* chop function start *)
      id
      |> String.fold
          ~init:""
          ~f:(fun acc c -> 
              if (c |> is_digit) 
              then (acc ^ (c |> Char.to_string)) 
              else "")
      |> function 
          | ""  -> (
            (id
              |> function
                  | ""  -> default_id
                  | s   -> s), 
            0) 
          | s   -> (
            (id 
              |> String.chop_prefix ~prefix:s
              |> function 
                  | Some s  -> s 
                  | None    -> default_id), 
            (Stdlib.int_of_string s))
      (* chop function end *)
    end in
    fun id -> begin
    (* new_inv_id function start *)
    let (id', num) = chop id in
    let num' = if num >= !inv_num then num else !inv_num in
    inv_num := num' + 1; id' ^ (num' |> string_of_int)
    (* new_inv_id function end *)
  end
%}

(* Token - Value *)
%token <string> IDENT
%token <int> NUMBER
%token <bool> BOOLEAN

(* Token - System and Keyboard Symbol *)
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token ASSERT HASH SEMICOLON COLON COMMA DOT MID
%token EOF

(* Token - Other Symbol *)
%token FNOT IMPLY IFF AND OR
%token LE GE LT GT EQ NEQ ENOT
%token PLUS MINUS STAR SLASH
%token ASSIGN

(* Token - Keyword *)
%token PRE POST
%token FORALL EXISTS
%token INT BOOL
%token IF ELSE FOR WHILE DO RETURN BREAK SKIP
%token SORTED PARTITIONED

(* Priority *)
%right  DOT
%right  FNOT
%right  IMPLY IFF
%right  ENOT
%left   OR AND
%left   EQ NEQ LT GT LE GE
%left   PLUS MINUS
%left   STAR SLASH

(* Type of Pattern *)
%type   <Ty.t>          typ ttyp
%type   <Expr.t>        exp
%type   <Expr.var>      lv
%type   <Fmla.t>        fmla pdc
%type   <Inv.t>         inv
%type   <Rank.t option> rank
%type   <Stmt.t>        stmt astmt assign
%type   <Decl.t>        decl arg_decl
%type   <Decl.t list>   arg
%type   <Pgm.t>         pgm

(* Entrancy of Pattern *)
%start  <Pgm.t>         start

%%

start:
  | p=pgm EOF                             { p }

(******************************************************************************)
(******************************************************************************)
(* Type                                                                       *)
(******************************************************************************)
(******************************************************************************)

typ:
  | t1=ttyp                               { t1 }
  | t1=ttyp LBRACK RBRACK                 { Ty.T_arr t1 }

ttyp:
  | INT                                   { Ty.T_int }
  | BOOL                                  { Ty.T_bool }

(******************************************************************************)
(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)
(******************************************************************************)

exp:
  | LPAREN e1=exp RPAREN                  { e1 }
  | n1=NUMBER                             { Expr.E_int n1 }
  | b1=BOOLEAN                            { Expr.E_bool b1 }
  | l1=lv                                 { Expr.E_var l1 }
  | e1=exp PLUS e2=exp                    { Expr.E_add (e1, e2) }
  | e1=exp MINUS e2=exp                   { Expr.E_sub (e1, e2) }
  | e1=exp STAR e2=exp                    { Expr.E_mul (e1, e2) }
  | e1=exp SLASH e2=exp                   { Expr.E_div (e1, e2) }
  | MINUS e1=exp                          { Expr.E_neg e1 }
  | MID l1=lv MID                         { Expr.E_len l1 }
  | ENOT e1=exp                           { Expr.E_not e1 }
  | e1=exp EQ e2=exp                      { Expr.E_eq (e1, e2) }
  | e1=exp NEQ e2=exp                     { Expr.E_neq (e1, e2) }
  | e1=exp LT e2=exp                      { Expr.E_lt (e1, e2) }
  | e1=exp GT e2=exp                      { Expr.E_gt (e1, e2) }
  | e1=exp LE e2=exp                      { Expr.E_le (e1, e2) }
  | e1=exp GE e2=exp                      { Expr.E_ge (e1, e2) }
  | id1=IDENT LPAREN el2=separated_nonempty_list (COMMA, exp) RPAREN
                                          { Expr.E_call (id1, el2) }

lv:
  | id1=IDENT                             { Expr.V_var id1 }
  | v1=lv LBRACK e2=exp RBRACK            { Expr.V_arr (v1, e2) }

(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

fmla:
  | LPAREN f1=fmla RPAREN                 { f1 }
  | e1=exp                                { Fmla.F_exp e1 }
  | FNOT f1=fmla                          { Fmla.F_not f1 }
  | f1=fmla AND f2=fmla                   { Fmla.F_and [f1; f2] |> Fmla.flatten }
  | f1=fmla OR f2=fmla                    { Fmla.F_or [f1; f2]  |> Fmla.flatten }
  | f1=fmla IMPLY f2=fmla                 { Fmla.F_imply (f1, f2) }
  | f1=fmla IFF f2=fmla                   { Fmla.F_iff (f1, f2) }
  | FORALL v1=lv DOT f2=fmla              { Fmla.F_forall (v1, f2) }
  | EXISTS v1=lv DOT f2=fmla              { Fmla.F_exists (v1, f2) }
  | pdc1=pdc                              { pdc1 }

pdc:
  | SORTED LPAREN v1=lv COMMA e2=exp COMMA e3=exp RPAREN
                                          { Fmla.create_sorted (v1, e2, e3) }
  | PARTITIONED LPAREN v1=lv COMMA e2=exp COMMA e3=exp COMMA e4=exp COMMA e5=exp RPAREN
                                          { Fmla.create_partitioned (v1, e2, e3, e4, e5) }

(******************************************************************************)
(******************************************************************************)
(* Invariant                                                                  *)
(******************************************************************************)
(******************************************************************************)

inv:
  | ASSERT id1=IDENT COLON f2=fmla        { Inv.create f2 ~id:(new_inv_id id1) }
  | ASSERT COLON f1=fmla                  { Inv.create f1 ~id:(new_inv_id "") }

(******************************************************************************)
(******************************************************************************)
(* Ranking Function                                                           *)
(******************************************************************************)
(******************************************************************************)

rank:
  | HASH LPAREN el1=separated_nonempty_list (COMMA, exp) RPAREN
                                          { Some el1 }
  |                                       { None }

(******************************************************************************)
(******************************************************************************)
(* Statement                                                                  *)
(******************************************************************************)
(******************************************************************************)

stmt:
  | LBRACE sl1=nonempty_list (astmt) RBRACE
                                          { Stmt.S_seq sl1 |> Stmt.flatten }
  | LBRACE RBRACE                         { Stmt.S_skip }
  | a1=astmt                              { a1 }

astmt:
  | SKIP SEMICOLON                        { Stmt.S_skip }
  | v1=lv ASSIGN e2=exp SEMICOLON         { Stmt.S_assign (v1, e2) }
  | d1=decl ASSIGN e2=exp SEMICOLON       { Stmt.S_assign ((Expr.V_var d1.id), e2) }
  | decl SEMICOLON                        { Stmt.S_skip }
  | IF LPAREN e1=exp RPAREN s2=stmt       { Stmt.S_if (e1, s2, Stmt.S_skip) }
  | IF LPAREN e1=exp RPAREN s2=stmt ELSE s3=stmt
                                          { Stmt.S_if (e1, s2, s3) }
  | WHILE i1=inv r2=rank LPAREN e3=exp RPAREN s4=stmt
  | WHILE r2=rank i1=inv LPAREN e3=exp RPAREN s4=stmt
                                          { Stmt.S_while (i1, r2, e3, s4) }
  | DO s1=stmt WHILE i2=inv r3=rank LPAREN e4=exp RPAREN
  | DO s1=stmt WHILE r3=rank i2=inv LPAREN e4=exp RPAREN
  | DO s1=stmt WHILE LPAREN e4=exp RPAREN i2=inv r3=rank
  | DO s1=stmt WHILE LPAREN e4=exp RPAREN r3=rank i2=inv
                                          { Stmt.S_seq [s1; (Stmt.S_while (i2, r3, e4, s1))] }
  | FOR i1=inv r2=rank LPAREN a3=assign SEMICOLON e4=exp SEMICOLON a5=assign RPAREN s6=stmt
  | FOR r2=rank i1=inv LPAREN a3=assign SEMICOLON e4=exp SEMICOLON a5=assign RPAREN s6=stmt 
                                          { Stmt.S_seq [a3; (Stmt.S_while (i1, r2, e4, (Stmt.S_seq [s6; a5])))] }
  | RETURN e1=exp SEMICOLON               { Stmt.S_return e1 }
  | BREAK SEMICOLON                       { Stmt.S_break }

assign:
  | v1=lv ASSIGN e2=exp                   { Stmt.S_assign (v1, e2) }
  | d1=decl ASSIGN e2=exp                 { Stmt.S_assign ((Expr.V_var d1.id), e2) }
  | decl                                  { Stmt.S_skip }

(******************************************************************************)
(******************************************************************************)
(* Declaration of a Variable                                                  *)
(******************************************************************************)
(******************************************************************************)

decl:
  | t1=typ id2=IDENT                      { let d = Decl.create t1 ~id:id2 in
                                            locals := d::!locals; d }

arg:
  | al1=separated_nonempty_list (COMMA, arg_decl)
                                          { al1 }

arg_decl:
  | t1=typ id2=IDENT                      { Decl.create t1 ~id:id2 }

(******************************************************************************)
(******************************************************************************)
(* Program                                                                    *)
(******************************************************************************)
(******************************************************************************)

pgm:
  | ASSERT PRE COLON? pre=fmla ASSERT POST COLON? post=fmla r1=rank t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
  | ASSERT PRE COLON? pre=fmla r1=rank ASSERT POST COLON? post=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
  | ASSERT POST COLON? post=fmla ASSERT PRE COLON? pre=fmla r1=rank t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
  | ASSERT POST COLON? post=fmla r1=rank ASSERT PRE COLON? pre=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
  | r1=rank ASSERT PRE COLON? pre=fmla ASSERT POST COLON? post=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
  | r1=rank ASSERT POST COLON? post=fmla ASSERT PRE COLON? pre=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=stmt
                                          { Pgm.create s5 
                                                ~pre:(pre |> Inv.create ~id:"pre") 
                                                ~post:(post |> Inv.create ~id:"post") 
                                                ~rank:r1
                                                ~typ:t2 
                                                ~id:id3 
                                                ~args:arg4 
                                                ~locals:!locals }
