module Language.JS.Types where

data ObjectProperty = OPI Expression
                    | OPKV Expression Expression
                    | OP Expression
                    | OPM Expression
                    deriving (Show)

type IsPrefix = Bool

data SwitchCase = Case Expression | DefaultCase
                deriving (Show)

type ExpressionOpt = Maybe Expression
type StatementOpt = Maybe Statement

data ForStyle = ForIn  BindExpression Expression
              | ForInV String BindExpression Expression
              | ForOf  BindExpression Expression
              | ForOfV String BindExpression Expression
              | ForRegular (Maybe BindExpression) ExpressionOpt ExpressionOpt
              deriving (Show)

-- | import and export binds
-- * as identifier
-- A, ...
-- {x,...}
data ImportClause = Namespace Expression
                  | DefaultName Expression
                  | BindNames [Expression]
                  deriving (Show)

-- possible binds
-- <<expty>>
-- a
-- a=1
-- {a}
-- {a}={a:1}
-- ...a
-- ...{a}
-- ...[a]
data BindExpression = BindVar Expression (Maybe Expression)
                    | BindPattern Expression (Maybe Expression)
                    | BindRest Expression
                    deriving (Show)

data Expression = -- literals
  LThis
  | LNull
  | LI String
  | LN String
  | LS String
  | LTS String
  | LB Bool
  | RegExp String String
  | UnaryUpdate String IsPrefix Expression
  | Unary String Expression
  | Spread Expression
  | LA [Expression]
  | LO [ObjectProperty]
  | LP Expression
  | Condition Expression Expression Expression -- exp ? exp : exp
  | Assignment String Expression Expression
  | Operation String Expression Expression
    -- function expression
  | Function ExpressionOpt [BindExpression] Statement
  | Arrow (Either BindExpression [BindExpression]) Statement -- arrow function
    -- class expression
  | Class ExpressionOpt ExpressionOpt Statement
  | ClassProperty Expression Expression
  | PropertyMethod Expression [BindExpression] Statement
  | ClassStatic Expression
  | ClassGetMethod Expression
  | ClassSetMethod Expression
    -- async expression
  | Async Expression
    -- member expression
  | Dot Expression Expression
  | Acc Expression Expression
  | FCall Expression [Expression]
  | New Expression -- new + expression
    -- comma / sequence
  | Comma Expression Expression
    -- empty expression ;
  | Empty
  | Comment String
  | MultilineComment String
  deriving (Show)

data Statement = SExp Expression
               -- module statements
               | SImportFile Expression
               | SImport (Either ImportClause [ImportClause]) Expression
               | SRExport Expression Expression
               | SExport Statement
               | SExportDefault Expression
               -- function and class declaration
               | SC String ExpressionOpt Statement
               | SF String [BindExpression] Statement
               -- variable statements
               | SVariable String [BindExpression]
               -- iteration statements
               | SWhile [Expression] Statement
               | SDoWhile Statement [Expression]
               | SFor ForStyle Statement
               | SLabel Expression Statement -- identifier: statement
               | SDebugger
               -- control statements
               | SContinue ExpressionOpt
               | SBreak ExpressionOpt
               -- general block statement
               | SBlock [Statement]
               | SIf Expression Statement StatementOpt
               -- switch statement
               | SSwitch Expression [Statement]
               | SCase [SwitchCase] [Statement] -- cases + statementslist
               -- try/catch/finally/throw statement
               | SThrow Expression
               | STry Statement Statement StatementOpt
               | SCatch ExpressionOpt Statement
               | SFinally Statement
               -- return statement
               | SReturn Expression
               -- with statement
               | SWith Expression Statement
               deriving (Show)
