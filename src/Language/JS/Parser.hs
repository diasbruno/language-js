{-# LANGUAGE FlexibleContexts #-}
module Language.JS.Parser where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import qualified Text.Parsec as P
import Language.JS.Types
import Language.JS.Common
import Language.JS.Operators

-- | identifier
identifier = do h <- P.many (P.oneOf "_$")
                t <- P.many1 P.alphaNum
                return (h ++ t)

identB = P.try (LI <$> (do i <- identifier
                           case i `elem` reservedWords of
                             True -> P.unexpected "reserved word"
                             _    -> return i)) P.<?> "[identifier]"

-- | numbers
-- oct numbers are already covered by decimal.
numberB = LN <$> (hexN <|> decN) P.<?> "[number-literal]"
  where hexN = liftM2 (++) (P.try (P.string "0x")) (P.many1 (P.oneOf "0123456789abcdefABCDEF"))
        decN = do
          lead <- P.many1 P.digit
          fraction <- liftM2 (:) (P.char '.') (P.many P.digit) <|> return ""
          expo <- expoN
          return (lead ++ fraction ++ expo)
        expoN = liftM2 (:) (P.oneOf "eE") (P.many P.digit) <|> return ""

-- | booleans
boolB = P.try (boolA "true" <|> boolA "false") P.<?> "[boolean]"
  where
    boolA = fmap (LB . toHask) . keywordB
    toHask s | s == "true" = True
             | otherwise = False

-- | this
thisB = const LThis <$> keywordB "this" P.<?> "[this]"

-- | null
nullB = const LNull <$> keywordB "null" P.<?> "[null]"

stringA ctor wc p = ctor <$> (P.try (P.between (P.char wc) (P.char wc) (p wc)))

-- | strings literal
stringB = stringA LS '\"' allowed <|> stringA LS '\'' allowed P.<?> "[string-literal]"
  where
    allowed e = P.many (P.satisfy (\c -> c /= '\n' && c /= e))

-- | template strings
templateStringB = stringA LTS '`' allowed P.<?> "[template-string]"
  where
    allowed e = P.many (P.satisfy (\c -> c /= e))

regexB :: (P.Stream s m Char) => P.ParsecT s u m Expression
regexB = let re = (P.string "/" >> return "") <|>
                 (do es <- P.char '\\' -- escaped char
                     t <- P.anyToken
                     n <- re
                     return (es:t:n)) <|>
                 (liftM2 (:) P.anyToken re)
        in RegExp <$> ((P.char '/') *> re) <*> P.many (P.oneOf "mgi")

-- | array literal
arrayB = P.try (LA <$> brackets (commaSep (whiteSpaces *> checkSpread Spread (expressionNonEmpty False)))) P.<?> "[array]"

-- | parenthesis expression
parensB = LP <$> parens (whiteSpaces *> (expressionNonEmpty True)) P.<?> "[parenthesis]"

checkSpread ctor p = do i <- P.optionMaybe (keywordB "...")
                        case i of
                          Just _ -> ctor <$> p
                          Nothing -> p

formalParameter = whiteSpaces *> bindExpression <* whiteSpaces
                  P.<?> "[formal-parameters]"

-- | function expression
functionDeclB = Function <$> (keywordB "function" *> lexeme (P.optionMaybe identB))
                         <*> lexeme (parens (commaSep formalParameter))
                         <*> lexeme (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)))
                P.<?> "[function]"

-- | arrow expression (function)
afunctionB = P.try (Arrow <$> (lexeme (parens manyParams <|> singleParam) <* keywordB "=>")
                          <*> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements))
             P.<?> "[arrow-function]"
  where singleParam = Left <$> bindVar
        manyParams = Right <$> commaSep formalParameter

functionB = (afunctionB <|> functionDeclB)

propertyMethodDef = P.try (PropertyMethod <$> lexeme identB
                                          <*> lexeme (parens (commaSep formalParameter))
                                          <*> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements))))
                    P.<?> "[class-method-definition]"

classStaticDef = lexeme (keywordB "static") *> (ClassStatic <$> (propertyMethodDef <|> classPropertyDef))

classGetSetMethodDef = (keywordB "set" *> (ClassSetMethod <$> propertyMethodDef)) <|>
                       (keywordB "get" *> (ClassGetMethod <$> propertyMethodDef))
                       P.<?> "[class-get-set-definition]"

asyncMethodDef = keywordB "async" *> (Async <$> propertyMethodDef)
                 P.<?> "[async-definition]"

classPropertyDef = P.try (ClassProperty <$> (lexeme identB <* P.char '=' <* whiteSpaces)
                                        <*> lexeme (expressionNonEmpty False))
                   P.<?> "[class-property]"
classB = keywordB "class" *> (Class <$> (lexeme (P.optionMaybe identB))
                                    <*> P.optionMaybe (keywordB "extends" *> lexeme identB)
                                    <*> (SBlock <$> braces (whiteSpaces *> classBlock)))
         P.<?> "[class-expression]"
  where classBlock = P.many (lexeme (toStatement <$> classBlockDecls))
        classBlockDecls = (classPropertyDef
                            <|> asyncMethodDef
                            <|> classStaticDef
                            <|> classGetSetMethodDef
                            <|> propertyMethodDef)

-- | key and/or value property pair
kvB = do
  sp <- P.optionMaybe (keywordB "...")
  case sp of
    Just _ -> OPI . Spread <$> literals
    Nothing -> (OPM <$> (asyncMethodDef <|> classGetSetMethodDef <|> propertyMethodDef)) <|> (do
      k <- identB
      x <- P.try (P.lookAhead (P.oneOf ",:}"))
      case x of
        ':' -> P.try (OPKV k <$> (lexeme (P.char ':') *> lexeme (checkSpread Spread (expressionNonEmpty False)) P.<?> "[object-value-expression]"))
        _ -> return (OPI k))

-- | object literal
objectB = LO <$> lexeme (braces (P.sepBy (whiteSpaces *> kvB <* whiteSpaces) (P.char ','))) P.<?> "[object-literal]"

dotMember p = Dot p <$> (lexeme (P.char '.') *> identB) P.<?> "[dot-expression]"
accessor p = Acc p <$> brackets (whiteSpaces *> (expressionNonEmpty True) <* whiteSpaces) P.<?> "[array-expression]"
callExp p = FCall p <$> lexeme (parens (commaSep (whiteSpaces *> expressionNonEmpty False))) P.<?> "[function-call]"

-- | new
newB = const Nothing <$> keywordB "new" P.<?> "[new]"

memberExp (Just p) = (do dt <- (callExp p <|> dotMember p <|> accessor p) P.<?> "[member-expression]"
                         memberExp (Just dt)) <|> return p
memberExp Nothing = (New <$> expressions) P.<?> "[new-expression]"

literals = thisB <|> nullB <|> boolB
           <|> stringB <|> templateStringB
           <|> arrayB  <|> objectB <|> regexB
           <|> numberB <|> identB

primaryExp = literals <|> functionB <|> classB <|> parensB

maybeSemi = P.optional (P.char ';')

emptyExp = (const Empty) <$> (P.char ';') P.<?> "[empty-expressions]"

leftHandSideExp = (newB <|> (Just <$> lexeme primaryExp)) >>= memberExp P.<?> "left-hand-side-expression"

-- | expressions
expressions = emptyExp <|> expressionNonEmpty True P.<?> "[expressions]"

comment = P.try (Comment <$> (P.string "//" *> P.many (P.satisfy (\c -> c /= '\n'))))
multilineComment = P.try (MultilineComment <$> (P.between (P.string "/*") (P.string "*/") (P.many P.anyToken)))

expressionNonEmpty notComma = comment <|> multilineComment <|>
                              functionB <|> classB <|>
                              (operationExp notComma (expressionNonEmpty notComma) leftHandSideExp) <|>
                              primaryExp
                              P.<?> "[non-empty-expressions]"

toStatement :: Expression -> Statement
toStatement (Function (Just (LI a)) b c) = SF a b c
toStatement (Class (Just (LI a)) b c)    = SC a b c
toStatement a                            = SExp a

-- Statements

importNamespaceClause = Namespace <$> ((keywordB "*" *> keywordB "as") *> identB)
importBindClause = BindNames <$> braces (commaSep (whiteSpaces *> identB <* whiteSpaces))
importDefaultNameClause = DefaultName <$> lexeme identB

importManyClauses = commaSep1 (whiteSpaces *> (importBindClause <|> importDefaultNameClause))

importClauses = (importNamespaceClause >>= return . Left)
                <|> (importManyClauses >>=  return . Right)

importFileStatement = SImportFile <$> lexeme stringB
importStatement = SImport <$> (lexeme importClauses <* keywordB "from") <*> lexeme stringB
importStatements = keywordB "import" *> (importStatement <|> importFileStatement)
                   P.<?> "[import-statement]"

reexportStatement = P.try (SRExport <$> (lexeme (expressionNonEmpty False) <* keywordB "from") <*> lexeme stringB)
exportDefaultStatement = keywordB "default" *> (SExportDefault <$> expressionNonEmpty False)
exportStatement = SExport <$> statements
exportStatements = keywordB "export" *> (reexportStatement <|> exportDefaultStatement <|> exportStatement)
                   P.<?> "[export-statement]"

continueStatement = SContinue <$> (keywordB "continue" *> P.optionMaybe identB)
                    P.<?> "[continue-statement]"
breakStatement = SBreak <$> (keywordB "break" *> P.optionMaybe identB)
                 P.<?> "[break-statement]"

blockStatement allowedStmt = SBlock <$> (P.try (braces (whiteSpaces *> P.many allowedStmt <* whiteSpaces)))
                             P.<?> "[block-statement]"

ifStatement = SIf <$> (keywordB "if" *> lexeme parensB)
                  <*> lexeme (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements)
                  <*> P.optionMaybe (keywordB "else" *> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements))
              P.<?> "[if-statement]"

catchB = SCatch <$> (keywordB "catch" *> lexeme (P.optionMaybe parensB))
                <*> blockStatement statements
         P.<?> "[try/catch-statement]"

finallyB = SFinally <$> (keywordB "finally" *> blockStatement statements)
           P.<?> "[try/catch/finally-statement]"

tryStatement = STry <$> (keywordB "try" *> lexeme (blockStatement statements))
                    <*> catchB
                    <*> P.optionMaybe finallyB P.<?> "[try-statement]"

throwStatement = SThrow <$> (keywordB "throw" *> expressionNonEmpty False)
                 P.<?> "[throw-statement]"

returnStatement = SReturn <$> (keywordB "return" *> expressions)
                  P.<?> "[return-statement]"

bindVar = BindVar <$> lexeme identB <*> P.optionMaybe (P.notFollowedBy (keywordB "=>") *> (lexeme (P.char '=') *> (expressionNonEmpty False)))
bindPatternDecl = BindPattern <$> (lexeme (objectB <|> arrayB)) <*> P.optionMaybe (lexeme (P.char '=') *> (expressionNonEmpty False))
bindSpread = BindRest <$> (keywordB "..." *> leftHandSideExp)
bindExpression = (bindVar <|> bindPatternDecl <|> bindSpread) P.<?> "[var-binds]"

constVariableStatement = P.try (SVariable <$> (keywordB "const") <*> commaSep1 (whiteSpaces *> bindExpression <* whiteSpaces))
notConstVariableStatement = P.try (SVariable <$> (keywordB "let" <|> keywordB "var") <*> commaSep1 (whiteSpaces *> bindExpression <* whiteSpaces))
variableStatement = constVariableStatement <|> notConstVariableStatement P.<?> "[variable-statement]"

caseClause = lexeme ((caseB <|> defaultCase) <* (P.char ':')) P.<?> "[switch/case-expression]"
  where defaultCase = const DefaultCase <$> (keywordB "default")
        caseB       = Case <$> (keywordB "case" *> literals)

caseCase = SCase <$> lexeme (P.many1 caseClause)
                 <*> P.many (lexeme ((breakStatement <* maybeSemi) <|> statements))

caseBlock = braces (whiteSpaces *> P.many caseCase <* whiteSpaces)
switchStatement = SSwitch <$> (keywordB "switch" *> lexeme parensB)
                          <*> caseBlock
                  P.<?> "[switch-statement]"

debuggerStatement = const SDebugger <$> keywordB "debugger"
                    P.<?> "[debugger-statement]"

breakableStatement = blockStatement ((breakStatement <* maybeSemi) <|> statements) <|> statements

whileStatement = SWhile <$> (keywordB "while" *> lexeme (parens (P.many1 expressions)))
                        <*> breakableStatement
                 P.<?> "[while-statement]"

doWhileStatement = SDoWhile <$> (keywordB "do" *> lexeme breakableStatement)
                            <*> (keywordB "while" *> parens (P.many1 expressions))
                   P.<?> "[do/while-statement]"

forInVStyle = P.try (ForInV <$> lexeme (keywordB "let" <|> keywordB "const" <|> keywordB "var")
                            <*> bindExpression
                            <*> (keywordB "in" *> (expressionNonEmpty False)))
forOfVStyle = P.try (ForOfV <$> lexeme (keywordB "let" <|> keywordB "const" <|> keywordB "var")
                            <*> bindExpression
                            <*> (keywordB "of" *> expressionNonEmpty False ))
forInStyle = P.try (ForIn <$> bindExpression <*> (keywordB "in" *> expressionNonEmpty False))
forOfStyle = P.try (ForOf <$> bindExpression <*> (keywordB "of" *> expressionNonEmpty False))
forRegularStyle = ForRegular <$> P.try (P.optionMaybe bindExpression <* (P.char ';'))
                             <*> P.try (P.optionMaybe (expressionNonEmpty True) <* (P.char ';'))
                             <*> P.optionMaybe (expressionNonEmpty True)
forStyle = forInVStyle <|> forOfVStyle <|> forInStyle <|> forOfStyle <|> forRegularStyle P.<?> "[for-style]"
forStatement = SFor <$> lexeme (keywordB "for" *> (parens forStyle)) <*> breakableStatement

iterationStatement = forStatement <|> whileStatement <|> doWhileStatement

withStatement = SWith <$> (keywordB "with" *> lexeme (parens (expressionNonEmpty True)))
                      <*> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements)
                P.<?> "[with-statement]"

labelledStatement = SLabel <$> P.try (lexeme (identB <* P.char ':')) <*> statements
                    P.<?> "[labelled-statement]"

statements = ((blockStatement statements
               <|> ifStatement
               <|> iterationStatement
               <|> debuggerStatement
               <|> labelledStatement
               <|> continueStatement
               <|> tryStatement
               <|> throwStatement
               <|> returnStatement
               <|> switchStatement
               <|> withStatement
               <|> variableStatement
               <|> (fmap toStatement expressions)) <* maybeSemi) P.<?> "[statements]"

topLevelStatements = importStatements <|> exportStatements <|> statements

-- | parser
parseJs = P.many (whiteSpaces *> lexeme (topLevelStatements <* maybeSemi <* whiteSpaces))

parse :: String -> String -> Either P.ParseError [Statement]
parse filename source = P.parse parseJs filename source

parseFromFile :: String -> IO (Either P.ParseError [Statement])
parseFromFile filename = readFile filename >>= return . P.parse parseJs filename
