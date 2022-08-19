data Expr = Lit Integer
            | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = (+) (eval a) (eval b)

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)

-- printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
-- printExpr a3
-- "1 + 9001 + 1 + 20001"