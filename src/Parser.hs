module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intseq :: Parser (Exp Int)
intseq = chainl1 intassgn (do {reservedOp lis ","; return (ESeq)})

intassgn :: Parser (Exp Int)
intassgn = try (do  v <- identifier lis
                    reservedOp lis "="
                    exp <- intexp
                    return (EAssgn v exp))
           <|> intexp

intexp :: Parser (Exp Int)
intexp = chainl1 intterm addOp

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor mulOp

intfactor :: Parser (Exp Int)
intfactor = try (do {n <- natural lis; return (Const (fromInteger n))})
            <|> try (do {v <- identifier lis; return (Var v)})
                <|> try (do {reservedOp lis "-"; f <- intfactor; return (UMinus f)})
                    <|> parens lis intseq

mulOp:: Parser (Exp Int -> Exp Int -> Exp Int)
mulOp = try (do { reservedOp lis "*"; return (Times)})
        <|> (do {reservedOp lis "/"; return (Div)})

addOp:: Parser (Exp Int -> Exp Int -> Exp Int)
addOp = try (do { reservedOp lis "+"; return (Plus)})
        <|> (do {reservedOp lis "-"; return (Minus)})

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 booland (do {reservedOp lis "||"; return (Or)})

booland :: Parser (Exp Bool)
booland = chainl1 boolnot (do {reservedOp lis "&&"; return (And)})

boolnot :: Parser (Exp Bool)
boolnot = try (do {reservedOp lis "!"; b <- boolterm; return (Not b)})
          <|> boolterm

boolterm :: Parser (Exp Bool)
boolterm = try (do {reserved lis "true"; return BTrue})
           <|> try (do {reserved lis "false"; return BFalse})
               <|> try (parens lis boolexp)
                   <|> do exp1 <- intseq
                          op <- try (do {reservedOp lis "=="; return Eq})
                                <|> try (do {reservedOp lis "!="; return NEq})
                                    <|> try (do {reservedOp lis "<"; return Lt})
                                        <|> (do {reservedOp lis ">"; return Gt})
                          exp2 <- intseq
                          return (op exp1 exp2)
                               
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commin (do {reservedOp lis ";"; return Seq})

commin :: Parser Comm
commin = try (do {reserved lis "skip"; return Skip})
         <|> try (do {v <- identifier lis;
                      reservedOp lis "="; exp <- intseq;
                      return (Let v exp)})
             <|> try (do {reserved lis "if"; b <- boolexp; reserved lis "then";
                          c1 <- braces lis comm; reserved lis "else"; c2 <- braces lis comm;
                          return (IfThenElse b c1 c2)})
                 <|> try (do {reserved lis "if"; b <- boolexp; reserved lis "then";
                              c <- braces lis comm; return (IfThen b c)})
                     <|>  do {reserved lis "repeat"; c <- comm;
                              reserved lis "until"; b <- boolexp;
                              return (Repeat c b)}

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
