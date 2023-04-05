open Ast
open Ast.IR1
open Ast.IR2
open Baselib


let collect_constant_strings code =
  let _counter = ref 0 in
  let ccs_value = function
    | V1.Nil -> V2.Nil, []
    | V1.Bool b -> V2.Bool b, []
    | V1.Int n -> V2.Int n, []
    | V1.Str t ->
      incr _counter;
      let l = "str_" ^ (string_of_int !_counter) in
      V2.Data l, [ l, t ]
  in
  let rec ccs_expr = function
    | IR1.Value v ->
      let v, ccs = ccs_value v in
      IR2.Value v, ccs
    | IR1.Var v -> IR2.Var v, []
    | IR1.Call (f, args) ->
      let args2 = List.map ccs_expr args in
      let ccs = List.flatten (List.map (fun (_, s) -> s) args2) in
      IR2.Call (f, List.map (fun (e, _) -> e) args2), ccs
  in
  let rec ccs_instr = function
    | IR1.Decl v -> IR2.Decl v, []
    | IR1.Return e ->
      let e2, ccs = ccs_expr e in
      IR2.Return e2, ccs
    | IR1.Expr e ->
      let e2, ccs = ccs_expr e in
      IR2.Expr e2, ccs
    | IR1.Assign (lv, e) ->
      let e2, ccs_e = ccs_expr e in
      IR2.Assign (lv, e2), ccs_e
    | IR1.Cond (t, y, n) ->
      let t2, ccs_t = ccs_expr t in
      let y2, ccs_y = ccs_block y in
      let n2, ccs_n = ccs_block n in
      IR2.Cond (t2, y2, n2), List.flatten [ ccs_t; ccs_y; ccs_n ]
    | IR1.While (t, e) ->
      let t2, ccs_t = ccs_expr t in
      let e2, ccs_e = ccs_block e in
      IR2.While (t2, e2), List.flatten [ ccs_t; ccs_e ]
  and ccs_block = function
    | [] -> [], []
    | i :: r ->
      let i2, ccs_i = ccs_instr i in
      let r2, ccs_r = ccs_block r in
      i2 :: r2, ccs_i @ ccs_r 
  in
  let ccs_def = function
    | IR1.Func (type_t,name, args, body) ->
      let body2, ccs = ccs_block body in
      IR2.Func (type_t,name, args, body2), ccs
  in
  let rec ccs_prog = function
    | [] -> [], []
    | d :: r ->
      let d2, ccs_d = ccs_def d in
      let r2, ccs_r = ccs_prog r in
      d2 :: r2,ccs_d@ccs_r
  in

  ccs_prog code
;;

let simplify code = collect_constant_strings code
