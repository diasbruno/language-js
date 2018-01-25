module Test.Iteration where

import Test.Hspec
import Test.Helpers

testIteration = do
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
