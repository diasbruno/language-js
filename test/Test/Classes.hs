module Test.Classes where

import Test.Helpers
import Test.Hspec

testClasses = do
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


testClassesDeclaration = do
  shouldBe (testTopLevelStatement "class a {}")
    "Right (SC \"a\" Nothing (SBlock []))"
