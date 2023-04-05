open Ast
open Ast.IR1
open Ast.V1
open Baselib
open Lexing

exception Error of string * Lexing.position

(* fonctions d'aide à la gestion des erreurs *)

let warn msg pos =
  Printf.eprintf "Warning on line %d col %d: %s.\n"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg


let expr_pos expr =
  match expr with
  | Syntax.Value n  -> n.pos
  | Syntax.Var v  -> v.pos
  | Syntax.Call c -> c.pos

let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type_t expected)
                  (string_of_type_t given),
                pos))

(* analyse sémantique *)


let analyze_value value env =
  match value with
  | Syntax.Nil n  -> Nil, Nil_t
  | Syntax.Int i  -> Int i.value, Int_t
  | Syntax.Str s  -> Str s.value, Str_t
  | Syntax.Bool b -> Bool b.value, Bool_t


let rec analyze_expr expr env =
  match expr with
  | Syntax.Var v ->
     if Env.mem v.name env then
       IR1.Var v.name, Env.find v.name env
     else
       raise (Error (Printf.sprintf "unbound variable '%s'" v.name,
                     v.pos))
  | Syntax.Value v ->  
      let a, b = (analyze_value v.value env) in
      IR1.Value a, b
  | Syntax.Call c ->
     match Env.find_opt c.func env with
     | Some (Func_t (rt, at)) ->
        if List.length at != List.length c.args then
          raise (Error (Printf.sprintf "expected %d arguments but given %d"
                          (List.length at) (List.length c.args), c.pos)) ;
        let args = List.map2 (fun eat a -> let aa, at = analyze_expr a env
                                           in if at = eat then aa
                                              else errt eat at (expr_pos a))
                     at c.args in
        IR1.Call (c.func, args), rt
     | Some _ -> raise (Error (Printf.sprintf "'%s' is not a function" c.func,
                                 c.pos))
       | None -> raise (Error (Printf.sprintf "undefined function '%s'" c.func,
                               c.pos))
  
  let rec analyze_instr instr env pile =
    match instr with
    | Syntax.Return r ->
       let ae, g = analyze_expr r.expr env in
       let t = Pile.pop pile in
       if (g != t) then
          errt t g r.pos
       else
          IR1.Return ae, env
    | Syntax.Decl d   -> 
       IR1.Decl d.name, Env.add d.name d.type_t env
    | Syntax.Assign a ->
      if (Env.mem a.var env) then
        let ae, et = analyze_expr a.expr env in
        let vt = Env.find a.var env in
        if (et = vt) then 
          IR1.Assign (a.var, ae), env
        else
          errt vt et a.pos
        else
            raise (Error (Printf.sprintf "Unbound variable %s" 
                          a.var, a.pos))
    | Syntax.Expr e ->
       let ae, et = analyze_expr e.expr env in
       IR1.Expr ae, env
    | Syntax.Cond c -> 
       let t, et  = analyze_expr c.test env in
       let y, et2 = analyze_block c.tblock env pile in
       let n, et3 = analyze_block c.fblock et2 pile in
       IR1.Cond (t, y, n), et3
    | Syntax.While w -> 
       let t, et  = analyze_expr w.test env in
       let e, et2 = analyze_block w.block env pile in
       IR1.While (t, e), et2
  
  
  and analyze_block block env pile =
    match block with
    | [] -> [], env
    | instr :: rest ->
       let ai, nenv = analyze_instr instr env pile in
       let b, s_env = analyze_block rest nenv pile in
       ai :: b , nenv
  
  let analyze_func func env pile =
    match func with
    | Syntax.Func f ->
      let args = [] in
      let block, nenv = analyze_block f.block env pile in
      IR1.Func (f.type_t, f.name, args, block), 
                if (Env.mem f.name env) then
                  raise (Error (Printf.sprintf "doesn't compile '%s' because this function exist with same name" 
                                f.name, f.pos))
                else
                  (Env.add f.name (Func_t (Nil_t, [])) env)
                  
  let rec analyze_prog prog env pile = 
    match prog with
    | [] -> [], env
    | func :: rest ->
       let ai, nenv = analyze_func func env pile in
       let bi, nenv2 = analyze_prog rest nenv pile in
       ai :: bi, nenv2
  
  let analyze parsed =
    analyze_prog parsed Baselib._types_ (Pile.create())
  