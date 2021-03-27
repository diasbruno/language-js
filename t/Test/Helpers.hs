module Test.Helpers where

import Language.JS.Parser hiding (parse)
import qualified Language.JS.Parser as JS
import Text.Parsec

testExpression = parse expressions "dummy.js" >>= return . show
testStatement = parse statements "dummy.js" >>= return . show
testTopLevelStatement = parse topLevelStatements "dummy.js" >>= return . show

testParseJs = JS.parse "dummy.js" >>= return . show
