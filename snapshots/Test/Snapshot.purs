module Test.Snapshot where

import Prelude

import Data.Foldable (for_)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.FS.Aff as FSA
import Node.FS.Sync as FS
import Node.Path as Path

main :: Effect Unit
main = do
  let outputEsDir = "output-es"
  launchAff_ do
    mods <- FSA.readdir outputEsDir
    for_ mods \mod ->
      for_ (String.stripPrefix (Pattern "Snapshot.") mod) \modName -> do
        let
          jsFile = Path.concat [ outputEsDir, mod, "index.js" ]
          snapshotFile = Path.concat [ "snapshots", "Snapshot", modName <> "-snapshot.js" ]
        whenM (liftEffect $ FS.exists snapshotFile) do
          FSA.unlink snapshotFile
        FSA.copyFile jsFile snapshotFile
