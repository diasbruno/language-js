module Test.Imports where

import Test.Hspec
import Test.Helpers

testImports = do
  shouldBe (testTopLevelStatement "import \"test.js\"")
    "Right (SImportFile (LS \"test.js\"))"

  shouldBe (testTopLevelStatement "import * as A from \"test.js\"")
    "Right (SImport (Left (Namespace (LI \"A\"))) (LS \"test.js\"))"

  shouldBe (testTopLevelStatement "import {x} from \"test.js\"")
    "Right (SImport (Right [BindNames [LI \"x\"]]) (LS \"test.js\"))"

  shouldBe (testTopLevelStatement "import B, {x} from \"test.js\"")
    "Right (SImport (Right [DefaultName (LI \"B\"),BindNames [LI \"x\"]]) (LS \"test.js\"))"
