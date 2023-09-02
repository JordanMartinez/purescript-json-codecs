module Test.Snapshot where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.FS.Aff as FSA
import Node.Path as Path

main :: Effect Unit
main = do
  void $ execSync "spago -x snapshots.dhall build --purs-args \"-g corefn\"" defaultExecSyncOptions
  void $ execSync "purs-backend-es build" $ defaultExecSyncOptions { cwd = Just "snapshots" }
  let
    snapshotsDir = "snapshots"
    outputEsDir = Path.concat [ snapshotsDir, "output-es" ]
  launchAff_ do
    mods <- FSA.readdir outputEsDir
    for_ mods \mod ->
      for_ (String.stripPrefix (Pattern "Snapshot.") mod) \modName -> do
        let
          jsFile = Path.concat [ outputEsDir, mod, "index.js" ]
          snapshotFile = Path.concat [ snapshotsDir, "Snapshot", modName <> "-snapshot.js" ]
        FSA.copyFile jsFile snapshotFile
