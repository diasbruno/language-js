module Test.Try where

import Test.Hspec
import Test.Helpers

testTry = do
  shouldBe (testStatement "try {} catch {}")
    "Right (STry (SBlock []) (SCatch Nothing (SBlock [])) Nothing)"

  shouldBe (testStatement "try {} catch {} finally {}")
    "Right (STry (SBlock []) (SCatch Nothing (SBlock [])) Nothing)"

  shouldBe (testStatement "try {} catch (e) {} finally {}")
    "Right (STry (SBlock []) (SCatch (Just (LP (LI \"e\"))) (SBlock [])) Nothing)"

  shouldBe (testStatement "return 1")
    "Right (SReturn (LN \"1\"))"
