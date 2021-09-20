module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s)
stepComm (Let var exp) s = let (n :!: s') = evalExp exp s
                           in (Skip :!: update var n s')
stepComm (Seq Skip c) s = (c :!: s)
stepComm (Seq com1 com2) s = let (com :!: s') = stepComm com1 s
                             in (Seq com com2 :!: s')
stepComm (IfThenElse bexp c1 c2) s = let (b :!: s') = evalExp bexp s
                                     in if b then (c1 :!: s') else (c2 :!: s')
stepComm loop@(Repeat c bexp) s = ((Seq c (IfThenElse bexp Skip loop)) :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var v) s = (lookfor v s :!: s)
evalExp (UMinus exp) s = let (n :!: s') = evalExp exp s in (-n :!: s')
evalExp (Plus exp1 exp2) s = let (n1 :!: s') = evalExp exp1 s
                                 (n2 :!: s'') = evalExp exp2 s'
                             in (n1 + n2 :!: s'')
evalExp (Minus exp1 exp2) s = let (n1 :!: s') = evalExp exp1 s
                                  (n2 :!: s'') = evalExp exp2 s'
                              in (n1 - n2 :!: s'')
evalExp (Times exp1 exp2) s = let (n1 :!: s') = evalExp exp1 s
                                  (n2 :!: s'') = evalExp exp2 s'
                              in (n1 * n2 :!: s'')
evalExp (Div exp1 exp2) s = let (n1 :!: s') = evalExp exp1 s
                                (n2 :!: s'') = evalExp exp2 s'
                            in (div n1 n2 :!: s'')
evalExp (EAssgn var exp) s = let (n :!: s') = evalExp exp s
                             in (n :!: update var n s')
evalExp (ESeq exp1 exp2) s = let (_ :!: s') = evalExp exp1 s
                             in evalExp exp2 s'

evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Lt exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                               (b2 :!: s'') = evalExp exp2 s'
                           in (b1 < b2 :!: s'')
evalExp (Gt exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                               (b2 :!: s'') = evalExp exp2 s'
                           in (b1 > b2 :!: s'')
evalExp (And exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                                (b2 :!: s'') = evalExp exp2 s'
                           in ((b1 && b2) :!: s'')
evalExp (Or exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                               (b2 :!: s'') = evalExp exp2 s'
                           in ((b1 || b2) :!: s'')
evalExp (Not exp) s = let (b :!: s') = evalExp exp s
                      in (not b :!: s')
evalExp (Eq exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                               (b2 :!: s'') = evalExp exp2 s'
                           in (b1 == b2 :!: s'') 
evalExp (NEq exp1 exp2) s = let (b1 :!: s') = evalExp exp1 s
                                (b2 :!: s'') = evalExp exp2 s'
                            in (b1 /= b2 :!: s'')