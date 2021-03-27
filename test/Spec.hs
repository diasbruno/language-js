module Main where

import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner
import Test.Helpers
import Test.MemberExpression
import Test.AssignmentAndOperation
import Test.Variables
import Test.If
import Test.Try
import Test.Switch
import Test.Functions
import Test.Classes
import Test.Iteration
import Test.Exports
import Test.Imports

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
    shouldBe (testExpression "'test'")
      "Right (LS \"test\")"
    shouldBe (testExpression "'\\''")
      "Right (LS \"'\")"
    shouldBe (testExpression "'\\n'")
      "Right (LS \"\\n\")"
    shouldBe (testExpression "'\n'")
      "Left \"dummy.js\" (line 1, column 2):\nunexpected \"\\n\"\nexpecting \"\\\\\" or \"'\""

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

  it "parens expression" $ do
    shouldBe (testExpression "(a,b)")
      "Right (LP (Comma (LI \"a\") (LI \"b\")))"

  it "new expression" $ testNewExpression

  it "function call expression" $ testFunctionCall

  it "dotted accesor" $ testDotAccessor

  it "array accessor" $ testArrayAccessor

  it "function expression" $ testFunctions

  it "class expression" $ testClasses

  it "unary expression" $ testUnaryOperations

  it "assignment expression" $ testAssignments

  it "operation expressions" $ testOperations

  it "spread expression" $ do
    shouldBe (testExpression "{...a}")
      "Right (LO [OPI (Spread (LI \"a\"))])"

  it "empty expression" $ do
    shouldBe (testExpression ";")
      "Right Empty"

testStatements = describe "Statements" $ do
  it "empty statement" $ do
    shouldBe (testStatement ";")
      "Right (SExp Empty)"

  it "if statement" $ testIf

  it "try statement" $ testTry

  it "throw statements" $ do
    shouldBe (testStatement "throw x;")
      "Right (SThrow (LI \"x\"))"

  it "variable statements" $ testVariables

  it "switch statement" $ testSwitch

  it "continue statement" $ do
    shouldBe (testStatement "continue")
      "Right (SContinue Nothing)"

    shouldBe (testStatement "continue x")
      "Right (SContinue (Just (LI \"x\")))"

  it "debugger statement" $ do
    shouldBe (testStatement "debugger")
      "Right SDebugger"

  it "iteration statement" $ testIteration

  it "with statement" $ do
    shouldBe (testStatement "with (a) {}")
      "Right (SWith (LI \"a\") (SBlock []))"

  it "labelled statement" $ do
    shouldBe (testStatement "a: x = 1")
      "Right (SLabel (LI \"a\") (SExp (Assignment \"=\" (LI \"x\") (LN \"1\"))))"

  it "import statement" $ testImports

  it "export statement" $ testExports

  it "function declaration" $ testFunctionDeclaration

  it "class declaration" $ testClassesDeclaration

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
