open Ast
open Mips

module Env = Map.Make(String)
module Pile = Stack (*crée pile*)

let _types_ = 
    List.fold_left
        (fun env (pris, typ) -> Env.add pris typ env)
        Env.empty
        [ "%add",  Func_t (Int_t , [ Int_t ; Int_t ])
         ;"%sub",  Func_t (Int_t , [ Int_t ; Int_t ])
         ;"%mul",  Func_t (Int_t , [ Int_t ; Int_t ])
         ;"%div",  Func_t (Int_t , [ Int_t ; Int_t ])
         ;"%mod",  Func_t (Int_t , [ Int_t ; Int_t ])
         
         ;"%puti",  Func_t (Nil_t , [ Int_t])
         ;"%puts",  Func_t (Nil_t , [ Str_t])
         ;"%putb",  Func_t (Nil_t , [ Bool_t])
         ;"%putnl", Func_t (Nil_t , [ ])

         ;"%eq" ,  Func_t (Bool_t, [ Int_t ; Int_t ])
         ;"%neq",  Func_t (Bool_t, [ Int_t ; Int_t ])

         ;"%gt" ,  Func_t (Bool_t, [ Int_t ; Int_t ])
         ;"%gte",  Func_t (Bool_t, [ Int_t ; Int_t ])
         ;"%lt" ,  Func_t (Bool_t, [ Int_t ; Int_t ])
         ;"%lte",  Func_t (Bool_t, [ Int_t ; Int_t ])

         ;"%or" ,  Func_t (Int_t, [ Bool_t ; Bool_t ])
         ;"%and",  Func_t (Int_t, [ Bool_t ; Bool_t ])

        ]

let builtins =
  [ 
    Label "%add"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Add (V0, T0, T1)
    ; Jr RA

    ;Label "%sub"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Sub (V0, T1, T0)
    ; Jr RA

    ; Label "%mul"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Mul (V0, T0, T1)
    ; Jr RA

    ; Label "%div"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Div (V0, T1, T0)
    ; Jr RA

    ; Label "%mod"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Div (V0, T1, T0)
    ; Mfhi V0
    ; Jr RA

    ; Label "%puti"
    ; Lw (A0, Mem (SP, 0))
    ; Li (V0, Syscall.print_int)
    ; Syscall
    ; Jr RA

    ; Label "%putnl"
    ; La (A0, (Lbl "nl"))
    ; Li (V0, Syscall.print_str)
    ; Syscall
    ; Jr RA

    ; Label "%geti"
    ; Lw (A0, Mem (SP, 0))
    ; Li (V0, Syscall.read_int)
    ; Syscall
    ; Jr RA

    ; Label "%puts"
    ; Move (A0, V0)
    ; Li (V0, Syscall.print_str)
    ; Syscall
    ; Jr RA

    ; Label "%putb"
    ; Move (A0, V0)
    ; Li (V0, Syscall.print_int)
    ; Syscall
    ; Jr RA
    (*  *)
    ; Label "%gt"
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Sgt (V0, T0, T1)
    ; Jr RA

    ; Label "%gte" 
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Sge (V0, T0, T1)
    ; Jr RA

    ; Label "%lt" 
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Slt (V0, T0, T1)
    ; Jr RA

    ; Label "%lte" 
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Sle (V0 ,T0,T1)
    ; Jr RA

    ; Label "%eq" 
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Seq (V0,T0,T1) 
    ; Jr RA

    ; Label "%neq"
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Sne (V0, T0, T1)
    ; Jr RA

    ; Label "%and" 
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; And (V0, T0, T1)
    ; Jr RA

    ; Label "%or"  
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Or (V0, T0, T1)
    ; Jr RA

    ; Label "%Xor"  
    ; Lw (T0, Mem (SP, 4))
    ; Lw (T1, Mem (SP, 0))
    ; Or (V0, T0, T1)
    ; Jr RA
  ]