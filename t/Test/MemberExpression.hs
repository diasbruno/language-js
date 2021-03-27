module Test.MemberExpression where

import Test.Hspec
import Test.Helpers

testNewExpression = do
    shouldBe (testExpression "new A")
      "Right (New (LI \"A\"))"

    shouldBe (testExpression "new A()")
      "Right (New (FCall (LI \"A\") []))"

testFunctionCall = do
    shouldBe (testExpression "a()")
      "Right (FCall (LI \"a\") [])"

    shouldBe (testExpression "a.b()")
      "Right (FCall (Dot (LI \"a\") (LI \"b\")) [])"

    shouldBe (testExpression "a().b")
      "Right (Dot (FCall (LI \"a\") []) (LI \"b\"))"

    shouldBe (testExpression "a(1).b")
      "Right (Dot (FCall (LI \"a\") [LN \"1\"]) (LI \"b\"))"

testDotAccessor = do
    shouldBe (testExpression "a.b")
      "Right (Dot (LI \"a\") (LI \"b\"))"

testArrayAccessor = do
    shouldBe (testExpression "a[0]")
      "Right (Acc (LI \"a\") (LN \"0\"))"

    shouldBe (testExpression "a[b.x]")
      "Right (Acc (LI \"a\") (Dot (LI \"b\") (LI \"x\")))"

    shouldBe (testExpression "a[b.x].c")
      "Right (Dot (Acc (LI \"a\") (Dot (LI \"b\") (LI \"x\"))) (LI \"c\"))"

    shouldBe (testExpression "a.c[b.x]")
      "Right (Acc (Dot (LI \"a\") (LI \"c\")) (Dot (LI \"b\") (LI \"x\")))"
