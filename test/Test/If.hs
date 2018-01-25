module Test.If where

import Test.Hspec
import Test.Helpers

testIf = do
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
