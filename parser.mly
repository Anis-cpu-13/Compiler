%{
  open Ast
  open Ast.Syntax
%}

(*Operators*)
%token Ladd Lsub Lmul Ldiv Lmod Lassign Lif Lelse Lwhile
%token Lgt Lgte Llt Llte Leq Lneq Land Lor 

(*Punctuations*)
%token Lsc Lopar Lcpar Locbra Lccbra

(*Keywords*)
%token Lreturn Lend Lprint_int Lprint_str Lprint_bool Lprint_nl

(*Values*)
%token <int> Lint
%token <bool> Lbool
%token <string> Lvar
%token <string> Lstring
%token <Ast.type_t> Ltype

(*Priority*)
%left Lor
%left Land


%left Leq
%left Lneq
%left Llt
%left Lgt
%left Llte
%left Lgte

%left Lmod
%left Ladd
%left Lsub
%left Lmul
%left Ldiv

(*Point d'entrée*)
%start start

(*Type de retour*)
%type <Ast.Syntax.prog> start

%%

start:
| prog Lend { $1 }

prog:
| def      { [$1] }
| def prog { $1::$2 }
;

(*Function*)
def:
| Ltype; Lvar; Lopar; Lcpar; Locbra; instrs; Lccbra{
  Func { type_t= $1; name = $2 ; args= [] ; block = $6 ;pos=$startpos($2) }
}
;

(*Instructions*)
instrs:
| instr        { [$1] }
| instr instrs { $1::$2 }
| Lend         { [] }
;

(*Instruction*)
instr:
| Lreturn; e = expr; Lsc; {
    Return { expr=e ; pos=$startpos($1) }
}
| Lvar; Lassign; expr; Lsc {
    Assign { var=$1 ; expr=$3 ; pos=$startpos($1) }
}
| Ltype; Lvar; Lsc {
    Decl { name=$2 ; type_t=$1 ; pos=$startpos($2) }
}
| expr; Lsc { 
    Expr { expr=$1 ; pos=$startpos($1) }
}
| Lif; Lopar; expr; Lcpar; Locbra; instrs; Lccbra; Lelse; Locbra; instrs; Lccbra {
    Cond { test=$3 ; tblock=$6 ; fblock=$10 ; pos=$startpos($1) }
}
| Lwhile; Lopar; expr; Lcpar; Locbra; instrs; Lccbra {
    While { test=$3 ; block=$6 ; pos=$startpos($1) }
}
;

(*Expression*)
expr:
| value{
    Value { value=$1 ; pos=$startpos($1) }
}
| operator { $1 }
| Lvar { 
    Var { name=$1; pos=$startpos($1) }
}
;

(*Operators*)
operator:
| expr ; Ladd ; expr {
    Call { func="_add" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lsub ; expr {
    Call { func="_sub" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lmul ; expr {
    Call { func="_mul" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lmod ; expr {
    Call { func="_mod" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Ldiv ; expr {
    Call { func="_div" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lgt ; expr {
    Call { func="_gt"  ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lgte ; expr {
    Call { func="_gte" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Llt ; expr {
    Call { func="_lt"  ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Llte; expr {
    Call { func="_lte" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Leq ; expr {
    Call { func="_eq"  ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lneq ; expr {
    Call { func="_neq" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Land ; expr {
    Call { func="_and" ; args=[$1;$3] ; pos=$startpos($2) }
}
| expr ; Lor ; expr {
    Call { func="_or"  ; args=[$1;$3] ; pos=$startpos($2) }
}
| Lprint_int; Lopar; expr; Lcpar {
    Call { func="puti" ; args=[$3] ; pos=$startpos($3) }
}
| Lprint_str; Lopar; expr; Lcpar {
    Call { func="puts" ; args=[$3] ; pos=$startpos($3) }
}
| Lprint_bool; Lopar; expr; Lcpar {
    Call { func="putb" ; args=[$3] ; pos=$startpos($3) }
}
| Lprint_nl; Lopar; Lcpar {
    Call { func="putnl" ; args=[] ; pos=$startpos($1) }
}
| Lvar; Lopar; Lcpar {
    Call { func=$1; args=[]; pos=$startpos($1) }
}
;

(*Values*)
value:
| Lint     { Int  { value=$1 ; pos=$startpos($1) } }
| Lbool    { Bool { value=$1 ; pos=$startpos($1) } }
| Lstring  { Str  { value=$1 ; pos=$startpos($1) } }
;