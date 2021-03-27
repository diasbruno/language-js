module Test.Variables where

import Test.Hspec
import Test.Helpers

testVariables = do
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
