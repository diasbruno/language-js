module Test.Functions where

import Test.Hspec
import Test.Helpers

testFunctions = do
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

testFunctionDeclaration = do
  shouldBe (testTopLevelStatement "function a() {} function b() {}")
    "Right (SF \"a\" [] (SBlock []))"

  shouldBe (testTopLevelStatement "function a() { var i; return 1; }")
    "Right (SF \"a\" [] (SBlock [SVariable \"var\" [BindVar (LI \"i\") Nothing],SReturn (LN \"1\")]))"

  shouldBe (testTopLevelStatement "function a() { function b() { return () => 1; } return 1; }")
    "Right (SF \"a\" [] (SBlock [SF \"b\" [] (SBlock [SReturn (Arrow (Right []) (SExp (LN \"1\")))]),SReturn (LN \"1\")]))"
