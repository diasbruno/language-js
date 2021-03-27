module Test.Exports where

import Test.Hspec
import Test.Helpers

testExports = do
  shouldBe (testTopLevelStatement "export {}")
    "Right (SExport (SBlock []))"

  shouldBe (testTopLevelStatement "export default {}")
    "Right (SExportDefault (LO []))"

  shouldBe (testTopLevelStatement "export {} from \"test.js\"")
    "Right (SRExport (LO []) (LS \"test.js\"))"

  shouldBe (testTopLevelStatement "export const a = 1;")
    "Right (SExport (SVariable \"const\" [BindVar (LI \"a\") (Just (LN \"1\"))]))"
