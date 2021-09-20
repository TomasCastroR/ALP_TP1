module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of
                  Just n -> Right n
                  _ -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'
  
-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                     = Right (Skip :!: s)
stepComm (Let var exp) s            = case evalExp exp s of
                                        Right (n :!: s') -> Right (Skip :!: update var n s')
                                        Left error -> Left error
stepComm (Seq Skip c) s             = Right (c :!: s)
stepComm (Seq com1 com2) s          = case stepComm com1 s of
                                        Right (com :!: s') -> Right (Seq com com2 :!: s')
                                        Left error -> Left error
stepComm (IfThenElse bexp c1 c2) s  = case evalExp bexp s of
                                        Right (b :!: s') -> if b then Right (c1 :!: s')
                                                                 else Right (c2 :!: s')
                                        Left error -> Left error
stepComm loop@(Repeat c bexp) s     = case evalExp bexp s of
                                        Right (b :!: s') -> if b then Right (Skip :!: s')
                                                                  else Right ((Seq c loop) :!: s')
                                        Left error -> Left error

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s         = Right (n :!: s)
evalExp (Var v) s           = case lookfor v s of
                                Right n -> Right (n :!: s)
                                _ -> Left UndefVar
evalExp (UMinus exp) s      =  case evalExp exp s of
                                Right (n :!: s') -> Right (-n :!: s')
                                Left error -> Left error
evalExp (Plus exp1 exp2) s  = case evalExp exp1 s of
                                  Right (n1 :!: s') -> 
                                    case evalExp exp2 s' of
                                        Right (n2 :!: s'') -> Right (n1 + n2 :!: s'')
                                        Left error -> Left error
                                  Left error -> Left error
evalExp (Minus exp1 exp2) s = case evalExp exp1 s of
                                  Right (n1 :!: s') -> 
                                    case evalExp exp2 s' of
                                        Right (n2 :!: s'') -> Right (n1 - n2 :!: s'')
                                        Left error -> Left error
                                  Left error -> Left error
evalExp (Times exp1 exp2) s = case evalExp exp1 s of
                                    Right (n1 :!: s') -> 
                                      case evalExp exp2 s' of
                                        Right (n2 :!: s'') -> Right (n1 * n2 :!: s'')
                                        Left error -> Left error
                                    Left error -> Left error
evalExp (Div exp1 exp2) s   = case evalExp exp1 s of
                                  Right (n1 :!: s') -> 
                                    case evalExp exp2 s' of
                                      Right (n2 :!: s'') -> 
                                        case n2 of
                                          0 -> Left DivByZero
                                          _ -> Right (div n1 n2 :!: s'')
                                      Left error -> Left error
                                  Left error -> Left error
evalExp (EAssgn var exp) s  = case evalExp exp s of
                                Right (n :!: s') -> Right (n :!: update var n s')
                                Left error -> Left error
evalExp (ESeq exp1 exp2) s  = case evalExp exp1 s of
                                  Right (_ :!: s') -> evalExp exp2 s'
                                  Left error -> Left error

evalExp BTrue s           = Right (True :!: s)
evalExp BFalse s          = Right (False :!: s)
evalExp (Lt exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right (b1 < b2 :!: s'')
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Gt exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right (b1 > b2 :!: s'')
                                  Left error -> Left error
                              Left error -> Left error
evalExp (And exp1 exp2) s = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right ((b1 && b2) :!: s'')
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Or exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right ((b1 || b2) :!: s'')
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Not exp) s       = case evalExp exp s of
                              Right (b :!: s') -> Right (not b :!: s') 
                              Left error -> Left error
evalExp (Eq exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right (b1 == b2 :!: s'')
                                  Left error -> Left error
                              Left error -> Left error 
evalExp (NEq exp1 exp2) s = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s' of
                                  Right (b2 :!: s'') -> Right (b1 /= b2 :!: s'')
                                  Left error -> Left error
                              Left error -> Left error
