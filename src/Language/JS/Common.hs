{-# LANGUAGE FlexibleContexts #-}
module Language.JS.Common where

import qualified Text.Parsec as P

parens, braces, angles, brackets :: P.Stream s m Char =>
                                    P.ParsecT s u m a -> P.ParsecT s u m a
parens   = P.between (P.string "(") (P.string ")")
braces   = P.between (P.string "{") (P.string "}")
angles   = P.between (P.string "<") (P.string ">")
brackets = P.between (P.string "[") (P.string "]")

semi, comma, dot, colon :: P.Stream s m Char => P.ParsecT s u m String
semi  = P.string ";"
comma = P.string ","
dot   = P.string "."
colon = P.string ":"

dotSep, commaSep, semiSep :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m [a]
dotSep p = P.sepBy1 p dot
commaSep p = P.sepBy p comma
semiSep  p = P.sepBy p semi

commaSep1, semiSep1 :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m [a]
commaSep1 p = P.sepBy1 p comma
semiSep1  p = P.sepBy1 p semi

whiteSpaces :: P.Stream s m Char => P.ParsecT s u m String
whiteSpaces = P.many (P.oneOf " \n\t")

lexeme :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m a
lexeme p = p <* whiteSpaces

keywordB :: P.Stream s m Char => String -> P.ParsecT s u m String
keywordB = P.try . lexeme . P.string

-- | reserved words
reservedWords :: [String]
reservedWords = ["function", "return", "var", "let", "const",
                 "switch", "case", "break", "default",
                 "for", "while", "do", "in", "of",
                 "if", "else",
                 "delete", "void", "typeof",
                 "class", "extends", "staticn", "get", "set",
                 "async",
                 "import", "from", "export", "as",
                 "instanceof"
                ]
