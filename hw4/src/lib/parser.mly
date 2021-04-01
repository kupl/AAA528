%{
  module Lib = struct end
  
  open Adt

  let locals = Stdlib.ref []

  let create_one : pos:loc -> Expr.t
  = fun ~pos -> begin
    (* one function start *)
    Expr.create (E_int 1) ~pos 
    (* one function end *)
  end

  let read_pos : Lexing.position * Lexing.position -> loc
  = fun (startpos, endpos) -> begin
    (* read_pos function start *)
    Pos ( { col=(startpos.pos_cnum - startpos.pos_bol + 1); lin=startpos.pos_lnum; },
          { col=(endpos.pos_cnum - endpos.pos_bol + 1); lin=endpos.pos_lnum; })
    (* read_pos function end *)
  end
%}

(* Token - Value *)
%token <string> IDENT
%token <int> NUMBER
%token <bool> BOOLEAN

(* Token - System and Keyboard Symbol *)
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token ASSERT SEMICOLON COLON COMMA DOT MID
%token EOF

(* Token - Other Symbol *)
%token FNOT IMPLY IFF AND OR
%token LE GE LT GT EQ NEQ ENOT
%token PLUS MINUS STAR SLASH INCR DECR
%token ASSIGN

(* Token - Keyword *)
%token PRE
%token FORALL EXISTS
%token VOID INT BOOL
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
%type   <Ty.t>          typ_c
%type   <Ty.ty>         typ
%type   <Expr.t>        exp_c
%type   <Expr.exp>      exp
%type   <Expr.var>      lv
%type   <Fmla.t>        fmla_c pdc
%type   <Fmla.fmla>     fmla
%type   <Inv.t>         pre
%type   <Stmt.t>        stmt_c
%type   <Stmt.stmt>     stmt seq astmt
%type   <Decl.t>        decl_c arg_decl
%type   <Decl.t list>   arg
%type   <Func.t>        func
%type   <Pgm.t>         pgm

(* Entrancy of Pattern *)
%start  <Pgm.t>         start

%%

start:
  | p1=pgm                                { p1 }

(******************************************************************************)
(******************************************************************************)
(* Type                                                                       *)
(******************************************************************************)
(******************************************************************************)

typ_c:
  | t1=typ                                { t1 |> Ty.create ~pos:(read_pos $loc(t1)) }

typ:
  | VOID                                  { Ty.T_void }
  | INT                                   { Ty.T_int }
  | BOOL                                  { Ty.T_bool }
  | t1=typ LBRACK RBRACK                  { Ty.T_arr (t1, None) }
  | t1=typ LBRACK n2=NUMBER RBRACK        { Ty.T_arr (t1, Some n2) }

(******************************************************************************)
(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)
(******************************************************************************)

exp_c:
  | e1=exp                                { e1 |> Expr.create ~pos:(read_pos $loc(e1)) }

exp:
  | LPAREN e1=exp RPAREN                  { e1 }
  | n1=NUMBER                             { Expr.E_int n1 }
  | b1=BOOLEAN                            { Expr.E_bool b1 }
  | v1=lv                                 { Expr.E_var v1 }
  | e1=exp_c PLUS e2=exp_c                { Expr.E_add (e1, e2) }
  | e1=exp_c MINUS e2=exp_c               { Expr.E_sub (e1, e2) }
  | e1=exp_c STAR e2=exp_c                { Expr.E_mul (e1, e2) }
  | e1=exp_c SLASH e2=exp_c               { Expr.E_div (e1, e2) }
  | MINUS e1=exp_c                        { Expr.E_neg e1 }
  | MID l1=lv MID                         { Expr.E_len l1 }
  | ENOT e1=exp_c                         { Expr.E_not e1 }
  | e1=exp_c EQ e2=exp_c                  { Expr.E_eq (e1, e2) }
  | e1=exp_c NEQ e2=exp_c                 { Expr.E_neq (e1, e2) }
  | e1=exp_c LT e2=exp_c                  { Expr.E_lt (e1, e2) }
  | e1=exp_c GT e2=exp_c                  { Expr.E_gt (e1, e2) }
  | e1=exp_c LE e2=exp_c                  { Expr.E_le (e1, e2) }
  | e1=exp_c GE e2=exp_c                  { Expr.E_ge (e1, e2) }
  | id1=IDENT LPAREN el2=separated_nonempty_list (COMMA, exp_c) RPAREN
                                          { Expr.E_call (id1, el2) }

lv:
  | id1=IDENT                             { Expr.V_var id1 }
  | v1=lv LBRACK e2=exp_c RBRACK          { Expr.V_arr (v1, e2) }

(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

fmla_c:
  | f1=fmla                               { f1 |> Fmla.create ~pos:(read_pos $loc(f1)) |> Fmla.flatten }
  | pdc1=pdc                              { pdc1 }

fmla:
  | LPAREN f1=fmla RPAREN                 { f1 }
  | e1=exp_c                              { Fmla.F_exp e1 }
  | FNOT f1=fmla_c                        { Fmla.F_not f1 }
  | f1=fmla_c AND f2=fmla_c               { Fmla.F_and [f1; f2] }
  | f1=fmla_c OR f2=fmla_c                { Fmla.F_or [f1; f2] }
  | f1=fmla_c IMPLY f2=fmla_c             { Fmla.F_imply (f1, f2) }
  | f1=fmla_c IFF f2=fmla_c               { Fmla.F_iff (f1, f2) }
  | FORALL v1=lv DOT f2=fmla_c            { Fmla.F_forall (v1, f2) }
  | EXISTS v1=lv DOT f2=fmla_c            { Fmla.F_exists (v1, f2) }

pdc:
  | SORTED LPAREN v1=lv COMMA e2=exp_c COMMA e3=exp_c RPAREN
                                          { Fmla.create_sorted (v1, e2, e3) ~pos:(read_pos $loc) }
  | PARTITIONED LPAREN v1=lv COMMA e2=exp_c COMMA e3=exp_c COMMA e4=exp_c COMMA e5=exp_c RPAREN
                                          { Fmla.create_partitioned (v1, e2, e3, e4, e5) ~pos:(read_pos $loc) }

(******************************************************************************)
(******************************************************************************)
(* Statement                                                                  *)
(******************************************************************************)
(******************************************************************************)

stmt_c:
  | s1=stmt                               { s1 |> Stmt.create ~pos:(read_pos $loc(s1)) |> Stmt.flatten }

stmt:
  | s1=seq                                { s1 }
  | s1=astmt SEMICOLON                    { s1 }
  | IF LPAREN e1=exp_c RPAREN s2=stmt_c   { Stmt.S_if (e1, s2, (Stmt.create (S_skip) ~pos:(Unknown))) }
  | IF LPAREN e1=exp_c RPAREN s2=stmt_c ELSE s3=stmt_c
                                          { Stmt.S_if (e1, s2, s3) }
  | WHILE LPAREN e1=exp_c RPAREN s2=stmt_c
                                          { Stmt.S_while (e1, s2) }
  | DO s1=stmt_c WHILE LPAREN e2=exp_c RPAREN
                                          { Stmt.S_seq [s1; (Stmt.S_while (e2, s1) |> Stmt.create ~pos:(read_pos $loc))] }
  | FOR LPAREN a1=astmt SEMICOLON e2=exp_c SEMICOLON a3=astmt RPAREN s4=stmt_c
                                          { let s1 = a1 |> Stmt.create ~pos:(read_pos $loc(a1)) in
                                            let s3 = a3 |> Stmt.create ~pos:(read_pos $loc(a3)) in
                                            Stmt.S_seq [s1; (Stmt.S_while (e2, (Stmt.S_seq [s4; s3] |> Stmt.create ~pos:(read_pos $loc))) |> Stmt.create ~pos:(read_pos $loc))] }

seq:
  | LBRACE sl1=nonempty_list (stmt_c) RBRACE
                                          { Stmt.S_seq sl1 }
  | LBRACE RBRACE                         { Stmt.S_skip }

astmt:
  | SKIP                                  { Stmt.S_skip }
  | v1=lv INCR                            { let ev1 = Expr.create (E_var v1) ~pos:(read_pos $loc(v1)) in
                                            let e2 = Expr.create (E_add (ev1, (create_one ~pos:(read_pos $loc)))) ~pos:(read_pos $loc) in
                                            Stmt.S_assign (v1, e2) }
  | v1=lv DECR                            { let ev1 = Expr.create (E_var v1) ~pos:(read_pos $loc(v1)) in
                                            let e2 = Expr.create (E_sub (ev1, (create_one ~pos:(read_pos $loc)))) ~pos:(read_pos $loc) in
                                            Stmt.S_assign (v1, e2) }
  | v1=lv ASSIGN e2=exp_c                 { Stmt.S_assign (v1, e2) }
  | d1=decl_c ASSIGN e2=exp_c             { Stmt.S_assign ((Expr.V_var d1.d.id), e2) }
  | decl_c                                { Stmt.S_skip }
  | RETURN e1=exp_c                       { Stmt.S_return e1 }
  | BREAK                                 { Stmt.S_break }

(******************************************************************************)
(******************************************************************************)
(* Declaration of a Variable                                                  *)
(******************************************************************************)
(******************************************************************************)

decl_c:
  | t1=typ_c id2=IDENT                    { let d = Decl.create t1 ~id:id2 ~pos:(read_pos $loc) in
                                            locals := d::!locals; d }

arg:
  | al1=separated_list (COMMA, arg_decl)  { al1 }

arg_decl:
  | t1=typ_c id2=IDENT                    { Decl.create t1 ~id:id2 ~pos:(read_pos $loc) }

(******************************************************************************)
(******************************************************************************)
(* Function                                                                   *)
(******************************************************************************)
(******************************************************************************)

func:
  | i1=pre t2=typ_c id3=IDENT LPAREN arg4=arg RPAREN s5=stmt_c
                                          { Func.create s5 
                                                ~pre:i1
                                                ~typ:t2 
                                                ~id:id3 
                                                ~args:arg4 
                                                ~locals:!locals }

pre:
  | ASSERT PRE COLON? f1=fmla_c           { f1 |> Inv.create ~id:"pre" ~pos:(read_pos $loc) }

(******************************************************************************)
(******************************************************************************)
(* Program                                                                    *)
(******************************************************************************)
(******************************************************************************)

pgm:
  | f1=func EOF                           { f1 }
