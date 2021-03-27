module Test.AssignmentAndOperation where

import Test.Hspec
import Test.Helpers

testUnaryOperations = do
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

testAssignments = do
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

testOperations = do
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
