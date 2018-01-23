{-# LANGUAGE FlexibleContexts #-}
module Language.JS.Parser where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import qualified Text.Parsec as P
import Language.JS.Types
import Language.JS.Common
import Language.JS.Operators

-- | Identifier name.
identifierName =
  liftM2 (++) (P.many (P.oneOf "_$")) (P.many1 P.alphaNum)

-- | Parse identifier (no reserved words).
identifier =
  P.try (LI <$> (do i <- identifierName
                    case i `elem` reservedWords of
                      True -> P.unexpected "reserved word"
                      _    -> return i)) P.<?> "[identifier]"

-- | Parse numeric literal.
-- Here we don't distinguish between kinds.
numericLiteral =
  LN <$> (hexN <|> decN)
  P.<?> "[number-literal]"
  where hexN = liftM2 (++) (P.try (P.string "0x")) (P.many1 (P.oneOf "0123456789abcdefABCDEF"))
        decN = do
          lead <- P.many1 P.digit
          fraction <- liftM2 (:) (P.char '.') (P.many P.digit) <|> return ""
          expo <- expoN
          return (lead ++ fraction ++ expo)
        expoN = liftM2 (:) (P.oneOf "eE") (P.many P.digit) <|> return ""

-- | Parse boolean literal.
booleanLiteral =
  P.try (boolA "true" <|> boolA "false")
  P.<?> "[boolean]"
  where boolA = fmap (LB . toHask) . keywordB
        toHask s | s == "true" = True
                 | otherwise = False

-- | this identifier
thisIdent =
  const LThis <$> keywordB "this" P.<?> "[this]"

-- | null identifier
nullIdent =
  const LNull <$> keywordB "null" P.<?> "[null]"

-- | Parse string literal.
stringLiteral =
  buildExpression LS '\"' withoutNewLineAllowed
  <|> buildExpression LS '\'' withoutNewLineAllowed
  <|> buildExpression LTS '`' withNewLineAllowed
  P.<?> "[string-literal]"
  where
    withNewLineAllowed e = P.many (P.satisfy (\c -> c /= '\n' && c /= e))
    withoutNewLineAllowed e = P.many (P.satisfy (\c -> c /= e))
    buildExpression ctor wc p = ctor <$> P.try (P.between (P.char wc) (P.char wc) (p wc))

-- | Parse regular expression literal.
regexLiteral =
  let re = (P.string "/" >> return "") <|>
           (do es <- P.char '\\' -- escaped char
               t <- P.anyToken
               n <- re
               return (es:t:n)) <|>
           (liftM2 (:) P.anyToken re)
  in RegExp <$> ((P.char '/') *> re) <*> P.many (P.oneOf "mgi")

-- | Parse elision (aka ',' without a value on array).
elision =
  const Elision <$> keywordB ","
  P.<?> "elision"

-- | Parse many items on a array declaration.
arrayItems ls =
  (lexeme (elision <|> item) >>= \x -> arrayItems (ls ++ [x])) <|> return ls
  where item = checkSpread Spread (expressionNonEmpty False) <* P.optional (P.char ',')

-- | Parse array literal.
arrayLiteral =
  P.try (LA <$> brackets (betweenSpaces (arrayItems [])))
  P.<?> "[array]"

-- | key and/or value property pair.
objectBinds = do
  sp <- P.optionMaybe (keywordB "...")
  case sp of
    Just _ -> OPI . Spread <$> literals
    Nothing -> (OPM <$> (asyncMethodDef <|> classGetSetMethodDef <|> propertyMethodDef)) <|> (do
      k <- identifier
      x <- P.try (P.lookAhead (P.oneOf ",:}"))
      case x of
        ':' -> P.try (OPKV k <$> (lexeme (P.char ':') *> lexeme (checkSpread Spread (expressionNonEmpty False)) P.<?> "[object-value-expression]"))
        _ -> return (OPI k))

-- | Parse object literal.
-- objectLiteral :: P.ParsecT s u m Expression
objectLiteral =
  LO <$> lexeme (braces (P.sepBy (betweenSpaces objectBinds) (P.char ',')))
  P.<?> "[object-literal]"

-- | Parse parenthesis expression.
parensExpression =
  LP <$> parens (betweenSpaces (expressionNonEmpty True))
  P.<?> "[parenthesis]"

-- | Check for spread operation before parse 'p'.
checkSpread ctor p =
  do i <- P.optionMaybe (keywordB "...")
     case i of
       Just _ -> ctor <$> p
       Nothing -> p

-- | Parse used by function declarations.
formalParameter =
  betweenSpaces bindExpression
  P.<?> "[formal-parameters]"

-- | Parse function declaration
functionDeclaration =
  keywordB "function" *>
    (Function <$> lexeme (P.optionMaybe identifier)
              <*> lexeme (parens (commaSep formalParameter))
              <*> lexeme (SBlock <$> braces (betweenSpaces (P.many (lexeme statements)))))
  P.<?> "[function]"

-- | Prase arrow function declaration.
arrowFunctionDeclaration =
  P.try (Arrow <$> (lexeme (parens manyParams <|> singleParam) <* keywordB "=>")
               <*> blockOrStatements)
  P.<?> "[arrow-function]"
  where singleParam = Left <$> bindVar
        manyParams = Right <$> commaSep formalParameter

-- | Parse any kind of funcion declaration (function or arrow function).
functionExpression =
  arrowFunctionDeclaration <|> functionDeclaration

-- | Parse property method of a class or object literal.
propertyMethodDef =
  P.try (PropertyMethod <$> lexeme identifier
                        <*> lexeme (parens (commaSep formalParameter))
                        <*> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements))))
  P.<?> "[class-method-definition]"

-- | Parse a static property of a class.
classStaticDef =
  lexeme (keywordB "static") *>
    (ClassStatic <$> (propertyMethodDef <|> classPropertyDef))

-- | Parse a getter or setter method.
classGetSetMethodDef =
  (keywordB "set" *> (ClassSetMethod <$> propertyMethodDef)) <|>
  (keywordB "get" *> (ClassGetMethod <$> propertyMethodDef))
  P.<?> "[class-get-set-definition]"

-- | Check for a async property method.
asyncMethodDef =
  keywordB "async" *> (Async <$> propertyMethodDef)
  P.<?> "[async-definition]"

-- | Parse a class property definition.
classPropertyDef =
  P.try (ClassProperty <$> (lexeme identifier <* P.char '=' <* whiteSpaces)
                       <*> lexeme (expressionNonEmpty False))
  P.<?> "[class-property]"

-- | Parse a class declaration.
classDeclaration =
  keywordB "class" *> (Class <$> (lexeme (P.optionMaybe identifier))
                             <*> P.optionMaybe (keywordB "extends" *> lexeme identifier)
                             <*> (SBlock <$> braces (whiteSpaces *> classBlock)))
  P.<?> "[class-expression]"
  where classBlock = P.many (lexeme (toStatement <$> classBlockDecls))
        classBlockDecls = (classPropertyDef
                            <|> asyncMethodDef
                            <|> classStaticDef
                            <|> classGetSetMethodDef
                            <|> propertyMethodDef)

-- | Dot member.
dotMember p =
  Dot p <$> (lexeme (P.char '.') *> identifier)
  P.<?> "[dot-expression]"

-- | Array like accessor.
accessor p =
  Acc p <$> brackets (betweenSpaces (expressionNonEmpty True))
  P.<?> "[array-expression]"

-- | Function call.
functionCall p =
  FCall p <$> lexeme (parens (commaSep (whiteSpaces *> expressionNonEmpty False)))
  P.<?> "[function-call]"

-- | new
newIdent =
  const Nothing <$> keywordB "new" P.<?> "[new]"

-- | Parse member expression.
memberExpression (Just p) =
  (do dt <- (functionCall p <|> dotMember p <|> accessor p) P.<?> "[member-expression]"
      memberExpression (Just dt)) <|> return p
memberExpression Nothing =
  (New <$> expressions) P.<?> "[new-expression]"

-- | Parse literals.
literals =
  thisIdent
  <|> nullIdent
  <|> booleanLiteral
  <|> stringLiteral
  <|> arrayLiteral
  <|> objectLiteral
  <|> regexLiteral
  <|> numericLiteral
  <|> identifier

-- | Parse primary expressions.
primaryExpression =
  literals
  <|> functionDeclaration
  <|> classDeclaration
  <|> parensExpression

-- | Check for maybe semi.
-- TODO: There are some rules for expression termination...need to check that.
maybeSemi =
  P.optional (P.char ';')

-- | Parse a empty expression.
emptyExpression =
  (const Empty) <$> (P.char ';')
  P.<?> "[empty-expressions]"

-- | Parse rules for left hand side expression.
leftHandSideExpression =
  (newIdent <|> (Just <$> lexeme primaryExpression)) >>= memberExpression
  P.<?> "left-hand-side-expression"

-- | Parse expressions.
expressions =
  emptyExpression <|> expressionNonEmpty True P.<?> "[expressions]"

-- | Parse single line comment.
comment =
  P.try (Comment <$> (P.string "//" *> P.many (P.satisfy (\c -> c /= '\n'))))

-- | Parse multiline comment.
multilineComment =
  P.try (MultilineComment <$> (P.between (P.string "/*") (P.string "*/") (P.many P.anyToken)))

-- | Parse comment like an expression.
commentExpression =
  comment <|> multilineComment

-- | Parse expressions excluding emptyExpression.
expressionNonEmpty notComma =
  commentExpression
  <|> functionExpression
  <|> classDeclaration
  <|> (operationExpression notComma (expressionNonEmpty notComma) leftHandSideExpression)
  <|> primaryExpression
  P.<?> "[non-empty-expressions]"

-- | Convert a expression into a statement.
toStatement :: Expression -> Statement
toStatement (Function (Just (LI a)) b c) = SF a b c
toStatement (Class (Just (LI a)) b c)    = SC a b c
toStatement a                            = SExp a

-- Statements

-- | Parse import namespace clauses.
importNamespaceClause =
  Namespace <$> ((keywordB "*" *> keywordB "as") *> identifier)

-- | Parse import bind clauses.
importBindClause =
  BindNames <$> braces (commaSep (betweenSpaces identifier))

-- | Parse default clauses.
importDefaultNameClause =
  DefaultName <$> lexeme identifier

-- | Parse import clauses excluding namespace clause.
importManyClauses =
  commaSep1 (whiteSpaces *> (importBindClause <|> importDefaultNameClause))

-- | Parse all import clauses.
importClauses =
  (Left <$> importNamespaceClause) <|>
  (Right <$> importManyClauses)

-- | Parse import file statement.
importFileStatement =
  SImportFile <$> lexeme stringLiteral

-- | Parse import statement.
importStatement =
  SImport <$> (lexeme importClauses <* keywordB "from") <*> lexeme stringLiteral

-- | Parse import statements.
importStatements =
  keywordB "import" *> (importStatement <|> importFileStatement)
  P.<?> "[import-statement]"

reexportStatement = P.try (SRExport <$> (lexeme (expressionNonEmpty False) <* keywordB "from") <*> lexeme stringLiteral)
exportDefaultStatement = keywordB "default" *> (SExportDefault <$> expressionNonEmpty False)
exportStatement = SExport <$> statements

-- | Parse export statements.
exportStatements = keywordB "export" *> (reexportStatement <|> exportDefaultStatement <|> exportStatement)
                   P.<?> "[export-statement]"

-- | Parse continue statement.
continueStatement =
  keywordB "continue" *> (SContinue <$> (P.optionMaybe identifier))
  P.<?> "[continue-statement]"

-- | Parse break statement.
breakStatement =
  keywordB "break" *> (SBreak <$> (P.optionMaybe identifier))
  P.<?> "[break-statement]"

-- | Parse block statement.
blockStatement allowedStmt =
  SBlock <$> P.try (braces (betweenSpaces (P.many allowedStmt)))
  P.<?> "[block-statement]"

blockOrStatements =
  SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements

-- | Parse if statement.
ifStatement =
  keywordB "if" *> (SIf <$> (lexeme parensExpression)
                        <*> lexeme blockOrStatements
                        <*> P.optionMaybe (keywordB "else" *> blockOrStatements))
  P.<?> "[if-statement]"

-- | Parse catch part of try statement.
catchBlock =
  keywordB "catch" *> (SCatch <$> lexeme (P.optionMaybe parensExpression)
                              <*> blockStatement statements)
  P.<?> "[try/catch-statement]"

-- | Parse finally part of try statement.
finallyBlock =
  keywordB "finally" *> (SFinally <$> (blockStatement statements))
  P.<?> "[try/catch/finally-statement]"

-- | Parse try statement.
tryStatement =
  keywordB "try" *> (STry <$> lexeme (blockStatement statements)
                          <*> catchBlock
                          <*> P.optionMaybe finallyBlock)
  P.<?> "[try-statement]"

-- | Parse throw statement.
throwStatement =
  keywordB "throw" *> (SThrow <$> (expressionNonEmpty False))
  P.<?> "[throw-statement]"

-- | Parse return statement.
returnStatement =
  keywordB "return" *> (SReturn <$> expressions)
  P.<?> "[return-statement]"

bindVar =
  BindVar <$> lexeme identifier <*> P.optionMaybe (P.notFollowedBy (keywordB "=>") *> (lexeme (P.char '=') *> (expressionNonEmpty False)))
bindPatternDecl =
  BindPattern <$> (lexeme (objectLiteral <|> arrayLiteral)) <*> P.optionMaybe (lexeme (P.char '=') *> (expressionNonEmpty False))
bindSpread =
  BindRest <$> (keywordB "..." *> leftHandSideExpression)
bindExpression =
  (bindVar <|> bindPatternDecl <|> bindSpread) P.<?> "[var-binds]"

constVariableStatement =
  P.try (SVariable <$> (keywordB "const") <*> commaSep1 (betweenSpaces bindExpression))
notConstVariableStatement =
  P.try (SVariable <$> (keywordB "let" <|> keywordB "var") <*> commaSep1 (betweenSpaces bindExpression))

-- | Parse variable statement.
variableStatement =
  constVariableStatement <|> notConstVariableStatement P.<?> "[variable-statement]"

-- | Parse case clause switch statement.
caseClause =
  lexeme ((caseB <|> defaultCase) <* (P.char ':'))
  P.<?> "[switch/case-expression]"
  where defaultCase = const DefaultCase <$> (keywordB "default")
        caseB       = keywordB "case" *> (Case <$> literals)

-- | Parse case clause switch statement.
caseDeclaration =
  SCase <$> lexeme (P.many1 caseClause)
        <*> P.many (lexeme ((breakStatement <* maybeSemi) <|> statements))

-- | Parse switch statement.
switchStatement =
  keywordB "switch" *>
    (SSwitch <$> lexeme parensExpression
             <*> braces (betweenSpaces (P.many caseDeclaration)))
  P.<?> "[switch-statement]"

-- | Parse debugger statement.
debuggerStatement =
  const SDebugger <$> keywordB "debugger"
  P.<?> "[debugger-statement]"

-- | Parse breakable statement.
-- TODO: this parser can be improved to parse vaild javascript code
-- by passing to the break statement to subsequent statements.
breakableStatement =
  blockStatement ((breakStatement <* maybeSemi) <|> statements) <|> statements

-- | Parse while statement.
whileStatement =
  keywordB "while" *>
    (SWhile <$> lexeme (parens (P.many1 expressions))
            <*> breakableStatement)
  P.<?> "[while-statement]"

-- | parse do-while statement.
doWhileStatement =
  keywordB "do" *>
    (SDoWhile <$> lexeme breakableStatement
              <*> (keywordB "while" *> parens (P.many1 expressions)))
  P.<?> "[do/while-statement]"

forInVStyle =
  P.try (ForInV <$> lexeme (keywordB "let" <|> keywordB "const" <|> keywordB "var")
                <*> bindExpression
                <*> (keywordB "in" *> (expressionNonEmpty False)))
forOfVStyle =
  P.try (ForOfV <$> lexeme (keywordB "let" <|> keywordB "const" <|> keywordB "var")
                <*> bindExpression
                <*> (keywordB "of" *> expressionNonEmpty False ))
forInStyle =
  P.try (ForIn <$> bindExpression <*> (keywordB "in" *> expressionNonEmpty False))
forOfStyle =
  P.try (ForOf <$> bindExpression <*> (keywordB "of" *> expressionNonEmpty False))
forRegularStyle =
  ForRegular <$> P.try (P.optionMaybe bindExpression <* (P.char ';'))
             <*> P.try (P.optionMaybe (expressionNonEmpty True) <* (P.char ';'))
             <*> P.optionMaybe (expressionNonEmpty True)
forStyle =
  forInVStyle <|> forOfVStyle <|> forInStyle <|> forOfStyle <|> forRegularStyle P.<?> "[for-style]"

-- | Parse for statement.
forStatement =
  SFor <$> lexeme (keywordB "for" *> (parens forStyle)) <*> breakableStatement

-- | Parse iteration statements (for, white, do/while).
iterationStatement = forStatement <|> whileStatement <|> doWhileStatement

-- | Parse with statement.
withStatement =
  keywordB "with" *> (SWith <$> lexeme (parens (expressionNonEmpty True))
                            <*> (SBlock <$> braces (whiteSpaces *> P.many (lexeme statements)) <|> statements))
  P.<?> "[with-statement]"

-- | Parse labelled statement.
labelledStatement =
  SLabel <$> P.try (lexeme (identifier <* P.char ':')) <*> statements
  P.<?> "[labelled-statement]"

-- | Parse statements.
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
               <|> fmap toStatement expressions) <* maybeSemi) P.<?> "[statements]"

-- | Parse all statements allowed to be on top level.
-- This helps to not allow import and export expressions
-- in any other part of the code.
topLevelStatements = importStatements <|> exportStatements <|> statements

-- | parser
parseJs = P.many (betweenSpaces (topLevelStatements <* maybeSemi))

-- | Parse a script with a filename.
parse = P.parse parseJs

-- | Parse a script from a file. Just for convinience.
parseFromFile filename = P.parse parseJs filename <$> readFile filename
