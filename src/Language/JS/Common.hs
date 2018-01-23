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

whiteSpace :: P.Stream s m Char => P.ParsecT s u m Char
whiteSpace = P.oneOf " \n\t"

whiteSpaces :: P.Stream s m Char => P.ParsecT s u m String
whiteSpaces = P.many whiteSpace

betweenSpaces :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m a
betweenSpaces p = whiteSpaces *> p <* whiteSpaces

lexeme :: P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m a
lexeme p = p <* whiteSpaces

keywordB :: P.Stream s m Char => String -> P.ParsecT s u m String
keywordB = P.try . lexeme . P.string

-- | reserved words
reservedWords :: [String]
reservedWords = ["async", "await", "yield", "delete", "void", "typeof",
                 "instanceof", "new", "debugger",
                 "var", "let", "const",
                 "switch", "case", "break", "default",
                 "try", "catch", "finally",
                 "for", "while", "do", "in", "of", "continue",
                 "if", "else",
                 "with",
                 "function", "return",
                 "class", "extends", "static", "get", "set", "super",
                 "import", "from", "export", "as"
                ]
