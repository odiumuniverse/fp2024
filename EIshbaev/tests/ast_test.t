  $ ../bin/main.exe
  [(ExprLet (Rec, "fact", (ExprVar "n"),
      (Some (ExprCond (
               (ExprBinop (Eql, (ExprVar "n"), (ExprConst (ConstInt 1)))),
               (ExprConst (ConstInt 1)),
               (ExprBinop (Mul, (ExprVar "n"),
                  (ExprApp ((ExprVar "fact"),
                     (ExprBinop (Sub, (ExprVar "n"), (ExprConst (ConstInt 1))))
                     ))
                  ))
               )))
      ))
    ]
