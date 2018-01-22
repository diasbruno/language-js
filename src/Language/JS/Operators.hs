{-# LANGUAGE FlexibleContexts #-}
module Language.JS.Operators where

import qualified Text.Parsec.Expr as P
import qualified Text.Parsec as P
import Language.JS.Types
import Language.JS.Common

opNotFollowedBy op p = keywordB op  <* P.notFollowedBy p

table :: P.Stream s m Char => Bool -> P.ParsecT s u m Expression -> [[P.Operator s u m Expression]]
table withComma exp' = [[ P.Postfix (UnaryUpdate    <$> keywordB "++" <*> pure False                 )             -- 17
              , P.Postfix (UnaryUpdate    <$> keywordB "--" <*> pure False                 )]
             ,[ P.Prefix  (Unary          <$> P.try (opNotFollowedBy "!" (P.char '='))     )             -- 16
              , P.Prefix  (Unary          <$> keywordB "~"                                 )
              , P.Prefix  (Unary          <$> P.try (opNotFollowedBy "+" (P.oneOf "+="))   )
              , P.Prefix  (Unary          <$> P.try (opNotFollowedBy "-" (P.oneOf "-="))   )
              , P.Prefix  (UnaryUpdate    <$> keywordB "++" <*> pure True                  )
              , P.Prefix  (UnaryUpdate    <$> keywordB "--" <*> pure True                  )
              , P.Prefix  (Unary          <$> keywordB "typeof"                            )
              , P.Prefix  (Unary          <$> keywordB "void"                              )
              , P.Prefix  (Unary          <$> keywordB "delete"                            )
              , P.Prefix  (Unary          <$> keywordB "await"                             )]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "**" (P.char '='))    ) P.AssocRight] -- 15
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "*"  (P.oneOf "*="))  ) P.AssocLeft  -- 14
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy "/"  (P.oneOf "*="))  ) P.AssocLeft
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy "%"  (P.oneOf "/="))  ) P.AssocLeft]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "+"  (P.oneOf "+="))  ) P.AssocLeft  -- 13
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy "-"  (P.oneOf "-="))  ) P.AssocLeft]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "<<" (P.oneOf "="))   ) P.AssocLeft  -- 12
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy ">>" (P.oneOf ">="))  ) P.AssocLeft
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy ">>>" (P.char '='))   ) P.AssocLeft]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "<" (P.oneOf "<="))   ) P.AssocLeft  -- 11
              , P.Infix   (Operation      <$> keywordB "<="                                ) P.AssocLeft
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy ">" (P.oneOf ">="))   ) P.AssocLeft
              , P.Infix   (Operation      <$> keywordB ">="                                ) P.AssocLeft
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy "in" (P.char 's'))    ) P.AssocLeft
              , P.Infix   (Operation      <$> keywordB "instanceof"                        ) P.AssocLeft]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "==" (P.char '='))    ) P.AssocLeft  -- 10
              , P.Infix   (Operation      <$> P.try (opNotFollowedBy "!=" (P.char '='))    ) P.AssocLeft
              , P.Infix   (Operation      <$> keywordB "==="                               ) P.AssocLeft
              , P.Infix   (Operation      <$> keywordB "!=="                               ) P.AssocLeft]
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "&" (P.oneOf "&="))   ) P.AssocLeft]  --  9
             ,[ P.Infix   (Operation      <$> keywordB "^"                                 ) P.AssocLeft]  --  8
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "|" (P.oneOf "|="))   ) P.AssocLeft]  --  7
             ,[ P.Infix   (Operation      <$> P.try (opNotFollowedBy "&&" (P.char '='))    ) P.AssocLeft]  --  6
             ,[ P.Infix   (Operation      <$> keywordB "||"                                ) P.AssocLeft]  --  5
             ,[ P.Infix   (flip Condition <$> (keywordB "?" *> lexeme exp' <* keywordB ":")) P.AssocNone] --  4
             ,[ P.Infix   (Assignment     <$> P.try (opNotFollowedBy "=" (P.oneOf ">="))   ) P.AssocRight --  3
              , P.Infix   (Assignment     <$> keywordB "+="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "-="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "**="                               ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "*="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "/="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "%="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "<<="                               ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB ">>="                               ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB ">>>="                              ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "&="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "^="                                ) P.AssocRight
              , P.Infix   (Assignment     <$> keywordB "|="                                ) P.AssocRight]
             ,[ P.Prefix  (Unary          <$> keywordB "yield"                             )]              --  2
             ,if withComma then ([ P.Infix (Comma <$ keywordB ",") P.AssocLeft]) else []  --  1
             ]

-- withComma = isLiteral (used when parsing arrays, objects and parenthesis expression)
operationExpression withComma exp' p = P.buildExpressionParser (table withComma exp') (lexeme p) P.<?> "[operations]"
