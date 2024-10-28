(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EIshbaev_lib.Ast

let () =
  let fact : structure =
    [ ExprLet
        ( Rec
        , "fact"
        , ExprVar "n"
        , Some
            (ExprCond
               ( ExprBinop (Eql, ExprVar "n", ExprConst (ConstInt 1))
               , ExprConst (ConstInt 1)
               , ExprBinop
                   ( Mul
                   , ExprVar "n"
                   , ExprApp
                       ( ExprVar "fact"
                       , ExprBinop (Sub, ExprVar "n", ExprConst (ConstInt 1)) ) ) )) )
    ]
  in
  print_endline (show_structure fact)
;;
