> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction (Defun fname paramname body)
>  = [Define fname] ++ transExp body freeRegs ++ [Ret]
>  where
>   freeRegs = [resultReg,D2,D3,D4,D5,D6,D7]

Part (2): saving registers

> saveRegs :: [Register] -> [Instr]
> saveRegs regsNotInUse
>  = [(Mov (Reg r) Push) | r <- allRegs, r `notElem` regsNotInUse]

> restoreRegs :: [Register] -> [Instr]
> restoreRegs regsNotInUse
>  = reverse [(Mov Pop (Reg r)) | r <- allRegs, r `notElem` regsNotInUse]

Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
>
> transExp (Const i) (dst : _) = [Mov (ImmNum i) (Reg dst)]
> transExp (Var _) (dst : _) = [Mov (Reg paramReg) (Reg dst)]

> transExp (Minus e1 e2) (r1 : r2 : rest)
>  | weight e1 > weight e2 = transExp e1 (r1 : r2 : rest) ++ transExp e2 (r2 : rest) ++ [Sub (Reg r2) (Reg r1)]
>  | otherwise             = transExp e2 (r2 : r1 : rest) ++ transExp e1 (r1 : rest) ++ [Sub (Reg r2) (Reg r1)]

> transExp (Apply s e) rs@(dst:_)
>   = saveRegs rs ++ (transExp e rs) ++ [Mov (Reg dst) (Reg paramReg)]
>     ++ [Jsr s] ++ [Mov (Reg resultReg) (Reg dst)] ++ restoreRegs rs

> weight (Const _) = 1
> weight (Var _) = 1
> weight (Minus e1 e2) = min w1 w2
>  where
>    w1 = max (weight e1) (weight e2 + 1)
>    w2 = max (weight e1 + 1) (weight e2)
> weight (Apply _ e) = weight e + 1
