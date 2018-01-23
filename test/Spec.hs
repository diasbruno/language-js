import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner
import Text.Parsec (parse)
import Language.JS.Parser hiding (parse)
import qualified Language.JS.Parser as JS

testExpression = parse expressions "dummy.js" >>= return . show
testStatement = parse statements "dummy.js" >>= return . show
testTopLevelStatement = parse topLevelStatements "dummy.js" >>= return . show

testParseJs = JS.parse "dummy.js" >>= return . show

testExpressions :: Spec
testExpressions = describe "Parse literals:" $ do
  it "this" $ do
    shouldBe (testExpression "this")
      "Right LThis"

  it "undefined/null" $ do
    shouldBe (testExpression "null")
      "Right LNull"

  it "booleans" $ do
    shouldBe (testExpression "true")
      "Right (LB True)"

    shouldBe (testExpression "false")
      "Right (LB False)"

  it "ident" $ do
    shouldBe (testExpression "test")
      "Right (LI \"test\")"

  it "numbers" $ do
    shouldBe (testExpression "1")
      "Right (LN \"1\")"

    shouldBe (testExpression "1.10")
      "Right (LN \"1.10\")"

    shouldBe (testExpression "0.10e3")
      "Right (LN \"0.10e3\")"

    shouldBe (testExpression "0x11")
      "Right (LN \"0x11\")"

    shouldBe (testExpression "0b11")
      "Right (LN \"0b11\")"

  it "strings" $ do
    shouldBe (testExpression "\"test\"")
      "Right (LS \"test\")"
    shouldBe (testExpression "\'test\'")
      "Right (LS \"test\")"

  it "template string" $ do
    shouldBe (testExpression "`test`")
      "Right (LTS [TString \"test\"])"

    shouldBe (testExpression "`${\"a\"}`")
      "Right (LTS [TExpression (LS \"a\")])"

    shouldBe (testExpression "`test ${a + 1} test`")
      "Right (LTS [TString \"test \",TExpression (Operation \"+\" (LI \"a\") (LN \"1\")),TString \" test\"])"

    shouldBe (testExpression "`${a} test ${b}`")
      "Right (LTS [TExpression (LI \"a\"),TString \" test \",TExpression (LI \"b\")])"

  it "arrays" $ do
    shouldBe (testExpression "[]")
      "Right (LA [])"

    shouldBe (testExpression "[,]")
      "Right (LA [Elision])"

    shouldBe (testExpression "[1,2]")
      "Right (LA [LN \"1\",LN \"2\"])"

    shouldBe (testExpression "[a,2]")
      "Right (LA [LI \"a\",LN \"2\"])"

    shouldBe (testExpression "[a,...b]")
      "Right (LA [LI \"a\",Spread (LI \"b\")])"

    shouldBe (testExpression "[a,...[1, 2]]")
      "Right (LA [LI \"a\",Spread (LA [LN \"1\",LN \"2\"])])"

    shouldBe (testExpression "[a,,b]")
      "Right (LA [LI \"a\",Elision,LI \"b\"])"

  it "objects" $ do
    shouldBe (testExpression "{}")
      "Right (LO [])"

    shouldBe (testExpression "{a:1}")
      "Right (LO [OPKV (LI \"a\") (LN \"1\")])"

    shouldBe (testExpression "{b:2,c: \"d\"}")
      "Right (LO [OPKV (LI \"b\") (LN \"2\"),OPKV (LI \"c\") (LS \"d\")])"

    shouldBe (testExpression "{b:2, c:function() {}}")
      "Right (LO [OPKV (LI \"b\") (LN \"2\"),OPKV (LI \"c\") (Function Nothing [] (SBlock []))])"

    shouldBe (testExpression "{e}")
      "Right (LO [OPI (LI \"e\")])"

    shouldBe (testExpression "{e,f:1}")
      "Right (LO [OPI (LI \"e\"),OPKV (LI \"f\") (LN \"1\")])"

    shouldBe (testExpression "{e,...f}")
      "Right (LO [OPI (LI \"e\"),OPI (Spread (LI \"f\"))])"

    shouldBe (testExpression "{e,...{a:1}}")
      "Right (LO [OPI (LI \"e\"),OPI (Spread (LO [OPKV (LI \"a\") (LN \"1\")]))])"

    shouldBe (testExpression "{e,i:...{a:1}}")
      "Right (LO [OPI (LI \"e\"),OPKV (LI \"i\") (Spread (LO [OPKV (LI \"a\") (LN \"1\")]))])"

    shouldBe (testExpression "{e(){}}")
      "Right (LO [OPM (PropertyMethod (LI \"e\") [] (SBlock []))])"

    shouldBe (testExpression "{get e(){}}")
      "Right (LO [OPM (ClassGetMethod (PropertyMethod (LI \"e\") [] (SBlock [])))])"

    shouldBe (testExpression "{set e(){}}")
      "Right (LO [OPM (ClassSetMethod (PropertyMethod (LI \"e\") [] (SBlock [])))])"

    shouldBe (testExpression "{async e(){}}")
      "Right (LO [OPM (Async (PropertyMethod (LI \"e\") [] (SBlock [])))])"

  it "regular expression" $ do
    shouldBe (testExpression "/test\\/asdf/")
      "Right (RegExp \"test\\\\/asdf\" \"\")"

    shouldBe (testExpression "/test\\/asdf/gmi")
      "Right (RegExp \"test\\\\/asdf\" \"gmi\")"

    shouldBe (testExpression "/test\\/asdf/gmi.exec(test)")
      "Right (FCall (Dot (RegExp \"test\\\\/asdf\" \"gmi\") (LI \"exec\")) [LI \"test\"])"

  it "try dotted" $ do
    shouldBe (testExpression "a.b")
      "Right (Dot (LI \"a\") (LI \"b\"))"

  it "array accessor" $ do
    shouldBe (testExpression "a[0]")
      "Right (Acc (LI \"a\") (LN \"0\"))"

    shouldBe (testExpression "a[b.x]")
      "Right (Acc (LI \"a\") (Dot (LI \"b\") (LI \"x\")))"

    shouldBe (testExpression "a[b.x].c")
      "Right (Dot (Acc (LI \"a\") (Dot (LI \"b\") (LI \"x\"))) (LI \"c\"))"

    shouldBe (testExpression "a.c[b.x]")
      "Right (Acc (Dot (LI \"a\") (LI \"c\")) (Dot (LI \"b\") (LI \"x\")))"

  it "parens expression" $ do
    shouldBe (testExpression "(a,b)")
      "Right (LP (Comma (LI \"a\") (LI \"b\")))"

  it "function expression" $ do
    shouldBe (testExpression "function a() {}")
      "Right (Function (Just (LI \"a\")) [] (SBlock []))"

    shouldBe (testExpression "function() {}")
      "Right (Function Nothing [] (SBlock []))"

    shouldBe (testExpression "function(a, ...b) {}")
      "Right (Function Nothing [BindVar (LI \"a\") Nothing,BindRest (LI \"b\")] (SBlock []))"

    shouldBe (testExpression "function(a=1) {}")
      "Right (Function Nothing [BindVar (LI \"a\") (Just (LN \"1\"))] (SBlock []))"

    shouldBe (testExpression "function(a=1) { return 1; }")
      "Right (Function Nothing [BindVar (LI \"a\") (Just (LN \"1\"))] (SBlock [SReturn (LN \"1\")]))"

    shouldBe (testExpression "function(a=1) {\n   function b() {} }")
      "Right (Function Nothing [BindVar (LI \"a\") (Just (LN \"1\"))] (SBlock [SF \"b\" [] (SBlock [])]))"

    shouldBe (testExpression "()=>x")
      "Right (Arrow (Right []) (SExp (LI \"x\")))"

    shouldBe (testExpression "()=>{x}")
      "Right (Arrow (Right []) (SBlock [SExp (LI \"x\")]))"

    shouldBe (testExpression "x=>{x}")
      "Right (Arrow (Left (BindVar (LI \"x\") Nothing)) (SBlock [SExp (LI \"x\")]))"

  it "class expression" $ do
    shouldBe (testExpression "class {}")
      "Right (Class Nothing Nothing (SBlock []))"

    shouldBe (testExpression "class test {}")
      "Right (Class (Just (LI \"test\")) Nothing (SBlock []))"

    shouldBe (testExpression "class test extends A {}")
      "Right (Class (Just (LI \"test\")) (Just (LI \"A\")) (SBlock []))"

    shouldBe (testExpression "class extends A {}")
      "Right (Class Nothing (Just (LI \"A\")) (SBlock []))"

    shouldBe (testExpression "class extends A { x = 1 }")
      "Right (Class Nothing (Just (LI \"A\")) (SBlock [SExp (ClassProperty (LI \"x\") (LN \"1\"))]))"

    shouldBe (testExpression "class extends A { x() {} }")
      "Right (Class Nothing (Just (LI \"A\")) (SBlock [SExp (PropertyMethod (LI \"x\") [] (SBlock []))]))"

    shouldBe (testExpression "class { static x = 1 }")
      "Right (Class Nothing Nothing (SBlock [SExp (ClassStatic (ClassProperty (LI \"x\") (LN \"1\")))]))"

    shouldBe (testExpression "class { set x() { return 1 } }")
      "Right (Class Nothing Nothing (SBlock [SExp (ClassSetMethod (PropertyMethod (LI \"x\") [] (SBlock [SReturn (LN \"1\")])))]))"

    shouldBe (testExpression "class { get x() { return 1 } }")
      "Right (Class Nothing Nothing (SBlock [SExp (ClassGetMethod (PropertyMethod (LI \"x\") [] (SBlock [SReturn (LN \"1\")])))]))"

    shouldBe (testExpression "class { async x() { return 1 } }")
      "Right (Class Nothing Nothing (SBlock [SExp (Async (PropertyMethod (LI \"x\") [] (SBlock [SReturn (LN \"1\")])))]))"

  it "new expression" $ do
    shouldBe (testExpression "new A")
      "Right (New (LI \"A\"))"

    shouldBe (testExpression "new A()")
      "Right (New (FCall (LI \"A\") []))"

  it "call expression" $ do
    shouldBe (testExpression "a()")
      "Right (FCall (LI \"a\") [])"

    shouldBe (testExpression "a.b()")
      "Right (FCall (Dot (LI \"a\") (LI \"b\")) [])"

    shouldBe (testExpression "a().b")
      "Right (Dot (FCall (LI \"a\") []) (LI \"b\"))"

    shouldBe (testExpression "a(1).b")
      "Right (Dot (FCall (LI \"a\") [LN \"1\"]) (LI \"b\"))"

  it "unary expression" $ do
    shouldBe (testExpression "+a")
      "Right (Unary \"+\" (LI \"a\"))"

    shouldBe (testExpression "-a")
      "Right (Unary \"-\" (LI \"a\"))"

    shouldBe (testExpression "~a")
      "Right (Unary \"~\" (LI \"a\"))"

    shouldBe (testExpression "!a")
      "Right (Unary \"!\" (LI \"a\"))"

    shouldBe (testExpression "delete a")
      "Right (Unary \"delete\" (LI \"a\"))"

    shouldBe (testExpression "typeof a")
      "Right (Unary \"typeof\" (LI \"a\"))"

    shouldBe (testExpression "void a")
      "Right (Unary \"void\" (LI \"a\"))"

    shouldBe (testExpression "++a")
      "Right (UnaryUpdate \"++\" True (LI \"a\"))"

    shouldBe (testExpression "a++")
      "Right (UnaryUpdate \"++\" False (LI \"a\"))"

    shouldBe (testExpression "--a")
      "Right (UnaryUpdate \"--\" True (LI \"a\"))"

    shouldBe (testExpression "a--")
      "Right (UnaryUpdate \"--\" False (LI \"a\"))"

  it "spread expression" $ do
    shouldBe (testExpression "{...a}")
      "Right (LO [OPI (Spread (LI \"a\"))])"

  it "assignment expression" $ do
    shouldBe (testExpression "x = 1")
      "Right (Assignment \"=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x += 1")
      "Right (Assignment \"+=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x -= 1")
      "Right (Assignment \"-=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x *= 1")
      "Right (Assignment \"*=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x /= 1")
      "Right (Assignment \"/=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x %= 1")
      "Right (Assignment \"%=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x &= 1")
      "Right (Assignment \"&=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x |= 1")
      "Right (Assignment \"|=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x **= 1")
      "Right (Assignment \"**=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x >>= 1")
      "Right (Assignment \">>=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x <<= 1")
      "Right (Assignment \"<<=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x >>>= 1")
      "Right (Assignment \">>>=\" (LI \"x\") (LN \"1\"))"

    shouldBe (testExpression "x.a += 1")
      "Right (Assignment \"+=\" (Dot (LI \"x\") (LI \"a\")) (LN \"1\"))"

  it "operation expressions" $ do
     shouldBe (testExpression "a + b")
       "Right (Operation \"+\" (LI \"a\") (LI \"b\"))"

     shouldBe (testExpression "a - b")
       "Right (Operation \"-\" (LI \"a\") (LI \"b\"))"

     shouldBe (testExpression "a * b")
       "Right (Operation \"*\" (LI \"a\") (LI \"b\"))"

     shouldBe (testExpression "a / b")
       "Right (Operation \"/\" (LI \"a\") (LI \"b\"))"

     shouldBe (testExpression "a = b - c")
       "Right (Assignment \"=\" (LI \"a\") (Operation \"-\" (LI \"b\") (LI \"c\")))"

     shouldBe (testExpression "a + b - c")
       "Right (Operation \"-\" (Operation \"+\" (LI \"a\") (LI \"b\")) (LI \"c\"))"

     shouldBe (testExpression "a + x.a - x.c")
       "Right (Operation \"-\" (Operation \"+\" (LI \"a\") (Dot (LI \"x\") (LI \"a\"))) (Dot (LI \"x\") (LI \"c\")))"

     shouldBe (testExpression "a + x.a && x.c")
       "Right (Operation \"&&\" (Operation \"+\" (LI \"a\") (Dot (LI \"x\") (LI \"a\"))) (Dot (LI \"x\") (LI \"c\")))"

     shouldBe (testExpression "a == x.a + x.c")
       "Right (Operation \"==\" (LI \"a\") (Operation \"+\" (Dot (LI \"x\") (LI \"a\")) (Dot (LI \"x\") (LI \"c\"))))"

     shouldBe (testExpression "!a")
       "Right (Unary \"!\" (LI \"a\"))"

     shouldBe (testExpression "a = x.a && x.c")
       "Right (Assignment \"=\" (LI \"a\") (Operation \"&&\" (Dot (LI \"x\") (LI \"a\")) (Dot (LI \"x\") (LI \"c\"))))"

     shouldBe (testExpression "a instanceof c")
       "Right (Operation \"instanceof\" (LI \"a\") (LI \"c\"))"

     shouldBe (testExpression "typeof a == \"number\"")
       "Right (Operation \"==\" (Unary \"typeof\" (LI \"a\")) (LS \"number\"))"

     shouldBe (testExpression "x ? y : z")
       "Right (Condition (LI \"x\") (LI \"y\") (LI \"z\"))"

     shouldBe (testExpression "x == 1 ? y : z")
       "Right (Condition (Operation \"==\" (LI \"x\") (LN \"1\")) (LI \"y\") (LI \"z\"))"

     shouldBe (testExpression "a = x == 1 ? y + 1 : z && 6")
       "Right (Assignment \"=\" (LI \"a\") (Condition (Operation \"==\" (LI \"x\") (LN \"1\")) (Operation \"+\" (LI \"y\") (LN \"1\")) (Operation \"&&\" (LI \"z\") (LN \"6\"))))"

     shouldBe (testExpression "a.b() || x")
       "Right (Operation \"||\" (FCall (Dot (LI \"a\") (LI \"b\")) []) (LI \"x\"))"

     shouldBe (testExpression "(x)\n.y")
       "Right (Dot (LP (LI \"x\")) (LI \"y\"))"

  it "empty expression" $ do
    shouldBe (testExpression ";")
      "Right Empty"

testStatements = describe "Statements" $ do
  it "empty statement" $ do
    shouldBe (testStatement ";")
      "Right (SExp Empty)"

  it "if statement" $ do
    shouldBe (testStatement "if (1) x = 1")
      "Right (SIf (LP (LN \"1\")) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))) Nothing)"

    shouldBe (testStatement "if (1)\nx = 1")
      "Right (SIf (LP (LN \"1\")) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))) Nothing)"

    shouldBe (testStatement "if (1) {x = 1}")
      "Right (SIf (LP (LN \"1\")) (SBlock [SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))]) Nothing)"

    shouldBe (testStatement "if (1) {x = 1} else x = 2")
      "Right (SIf (LP (LN \"1\")) (SBlock [SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))]) (Just (SExp (Assignment \"=\" (LI \"x\") (LN \"2\")))))"

    shouldBe (testStatement "if (a in [1, 2]) {}")
      "Right (SIf (LP (Operation \"in\" (LI \"a\") (LA [LN \"1\",LN \"2\"]))) (SBlock []) Nothing)"

  it "try statement" $ do
    shouldBe (testStatement "try {} catch {}")
      "Right (STry (SBlock []) (SCatch Nothing (SBlock [])) Nothing)"

    shouldBe (testStatement "try {} catch {} finally {}")
      "Right (STry (SBlock []) (SCatch Nothing (SBlock [])) Nothing)"

    shouldBe (testStatement "try {} catch (e) {} finally {}")
      "Right (STry (SBlock []) (SCatch (Just (LP (LI \"e\"))) (SBlock [])) Nothing)"

    shouldBe (testStatement "return 1")
      "Right (SReturn (LN \"1\"))"

  it "throw statements" $ do
    shouldBe (testStatement "throw x;")
      "Right (SThrow (LI \"x\"))"

  it "variable statements" $ do
    shouldBe (testStatement "var x;")
      "Right (SVariable \"var\" [BindVar (LI \"x\") Nothing])"

    shouldBe (testStatement "let x;")
      "Right (SVariable \"let\" [BindVar (LI \"x\") Nothing])"

    shouldBe (testStatement "var x = 1")
      "Right (SVariable \"var\" [BindVar (LI \"x\") (Just (LN \"1\"))])"

    shouldBe (testStatement "var x = 1, y;")
      "Right (SVariable \"var\" [BindVar (LI \"x\") (Just (LN \"1\")),BindVar (LI \"y\") Nothing])"

    shouldBe (testStatement "const x = 1;")
      "Right (SVariable \"const\" [BindVar (LI \"x\") (Just (LN \"1\"))])"

    shouldBe (testStatement "const x = 1, y = 2;")
      "Right (SVariable \"const\" [BindVar (LI \"x\") (Just (LN \"1\")),BindVar (LI \"y\") (Just (LN \"2\"))])"

    shouldBe (testStatement "const {a} = {a:1};")
      "Right (SVariable \"const\" [BindPattern (LO [OPI (LI \"a\")]) (Just (LO [OPKV (LI \"a\") (LN \"1\")]))])"

  it "switch statement" $ do
    shouldBe (testStatement "switch (a) {}")
      "Right (SSwitch (LP (LI \"a\")) [])"

    shouldBe (testStatement "switch (a) { case 1: { return; } }")
      "Right (SSwitch (LP (LI \"a\")) [SCase [Case (LN \"1\")] [SBlock [SReturn Empty]]])"

    shouldBe (testStatement "switch (a) { case 1: { return; } break }")
      "Right (SSwitch (LP (LI \"a\")) [SCase [Case (LN \"1\")] [SBlock [SReturn Empty],SBreak Nothing]])"

    shouldBe (testStatement "switch (a) { case 1: default: { return; } }")
      "Right (SSwitch (LP (LI \"a\")) [SCase [Case (LN \"1\"),DefaultCase] [SBlock [SReturn Empty]]])"

    shouldBe (testStatement "switch (a) { case 1: { return 1; } case 2: default: { return; } }")
      "Right (SSwitch (LP (LI \"a\")) [SCase [Case (LN \"1\")] [SBlock [SReturn (LN \"1\")]],SCase [Case (LN \"2\"),DefaultCase] [SBlock [SReturn Empty]]])"

    shouldBe (testStatement "switch (a) { case 1: { return 1; } break; default: return; }")
      "Right (SSwitch (LP (LI \"a\")) [SCase [Case (LN \"1\")] [SBlock [SReturn (LN \"1\")],SBreak Nothing],SCase [DefaultCase] [SReturn Empty]])"

  it "continue statement" $ do
    shouldBe (testStatement "continue")
      "Right (SContinue Nothing)"

    shouldBe (testStatement "continue x")
      "Right (SContinue (Just (LI \"x\")))"

  it "debugger statement" $ do
    shouldBe (testStatement "debugger")
      "Right SDebugger"

  it "iteration statement" $ do
    shouldBe (testStatement "while (1) {}")
      "Right (SWhile [LN \"1\"] (SBlock []))"

    -- break is allowed here
    shouldBe (testStatement "while (1) {break}")
      "Right (SWhile [LN \"1\"] (SBlock [SBreak Nothing]))"

    shouldBe (testStatement "while (1) {\nbreak\n;}")
      "Right (SWhile [LN \"1\"] (SBlock [SBreak Nothing]))"

    shouldBe (testStatement "while (1) x = 1")
      "Right (SWhile [LN \"1\"] (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

    shouldBe (testStatement "do {} while (1)")
      "Right (SDoWhile (SBlock []) [LN \"1\"])"

    shouldBe (testStatement "do {break} while (1)")
      "Right (SDoWhile (SBlock [SBreak Nothing]) [LN \"1\"])"

    shouldBe (testStatement "do x = 1; while (1)")
      "Right (SDoWhile (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))) [LN \"1\"])"

    shouldBe (testStatement "for (;;) {}")
      "Right (SFor (ForRegular Nothing Nothing Nothing) (SBlock []))"

    shouldBe (testStatement "for (;;) {break}")
      "Right (SFor (ForRegular Nothing Nothing Nothing) (SBlock [SBreak Nothing]))"

    shouldBe (testStatement "for (;;) x = 1")
      "Right (SFor (ForRegular Nothing Nothing Nothing) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

    shouldBe (testStatement "for (x;;) x = 1")
      "Right (SFor (ForRegular (Just (BindVar (LI \"x\") Nothing)) Nothing Nothing) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

    shouldBe (testStatement "for (x;;x++) x = 1")
      "Right (SFor (ForRegular (Just (BindVar (LI \"x\") Nothing)) Nothing (Just (UnaryUpdate \"++\" False (LI \"x\")))) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

    shouldBe (testStatement "for (x;;x+=1) x = 1")
      "Right (SFor (ForRegular (Just (BindVar (LI \"x\") Nothing)) Nothing (Just (Assignment \"+=\" (LI \"x\") (LN \"1\")))) (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

    shouldBe (testStatement "for (;x&&y;) {}")
      "Right (SFor (ForRegular Nothing (Just (Operation \"&&\" (LI \"x\") (LI \"y\"))) Nothing) (SBlock []))"

    shouldBe (testStatement "for (x in a) {}")
      "Right (SFor (ForIn (BindVar (LI \"x\") Nothing) (LI \"a\")) (SBlock []))"

    shouldBe (testStatement "for (let x in a) {}")
      "Right (SFor (ForInV \"let\" (BindVar (LI \"x\") Nothing) (LI \"a\")) (SBlock []))"

    shouldBe (testStatement "for (x of a) {}")
      "Right (SFor (ForOf (BindVar (LI \"x\") Nothing) (LI \"a\")) (SBlock []))"

    shouldBe (testStatement "for (let x of a) {}")
      "Right (SFor (ForOfV \"let\" (BindVar (LI \"x\") Nothing) (LI \"a\")) (SBlock []))"

  it "with statement" $ do
    shouldBe (testStatement "with (a) {}")
      "Right (SWith (LI \"a\") (SBlock []))"

  it "labelled statement" $ do
    shouldBe (testStatement "a: x = 1")
      "Right (SLabel (LI \"a\") (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

  it "import statement" $ do
    shouldBe (testTopLevelStatement "import \"test.js\"")
      "Right (SImportFile (LS \"test.js\"))"

    shouldBe (testTopLevelStatement "import * as A from \"test.js\"")
      "Right (SImport (Left (Namespace (LI \"A\"))) (LS \"test.js\"))"

    shouldBe (testTopLevelStatement "import {x} from \"test.js\"")
      "Right (SImport (Right [BindNames [LI \"x\"]]) (LS \"test.js\"))"

    shouldBe (testTopLevelStatement "import B, {x} from \"test.js\"")
      "Right (SImport (Right [DefaultName (LI \"B\"),BindNames [LI \"x\"]]) (LS \"test.js\"))"

  it "export statement" $ do
    shouldBe (testTopLevelStatement "export {}")
      "Right (SExport (SBlock []))"

    shouldBe (testTopLevelStatement "export default {}")
      "Right (SExportDefault (LO []))"

    shouldBe (testTopLevelStatement "export {} from \"test.js\"")
      "Right (SRExport (LO []) (LS \"test.js\"))"

    shouldBe (testTopLevelStatement "export const a = 1;")
      "Right (SExport (SVariable \"const\" [BindVar (LI \"a\") (Just (LN \"1\"))]))"

  it "function declaration" $ do
    shouldBe (testTopLevelStatement "function a() {} function b() {}")
      "Right (SF \"a\" [] (SBlock []))"

    shouldBe (testTopLevelStatement "function a() { var i; return 1; }")
      "Right (SF \"a\" [] (SBlock [SVariable \"var\" [BindVar (LI \"i\") Nothing],SReturn (LN \"1\")]))"

    shouldBe (testTopLevelStatement "function a() { function b() { return () => 1; } return 1; }")
      "Right (SF \"a\" [] (SBlock [SF \"b\" [] (SBlock [SReturn (Arrow (Right []) (SExp (LN \"1\")))]),SReturn (LN \"1\")]))"

  it "class declaration" $ do
    shouldBe (testTopLevelStatement "class a {}")
      "Right (SC \"a\" Nothing (SBlock []))"

testAll :: Spec
testAll = do
  testExpressions
  testStatements

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig testAll
  when (summaryFailures summary == 0)
    exitSuccess
  exitFailure
