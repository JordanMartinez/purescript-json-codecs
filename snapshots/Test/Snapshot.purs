module Test.Snapshot where

import Prelude

import Data.Foldable (for_)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.FS.Aff as FSA
import Node.FS.Sync as FS
import Node.Path as Path

main :: Effect Unit
main = do
  launchAff_ do
    copyFile "output-es"
      (\outputEsDir mod _ -> Path.concat [ outputEsDir, mod, "index.js" ])
      (\_ _ modName -> Path.concat [ "snapshots", "Snapshot", modName <> "-snapshot.js" ])
    copyFile "output"
      (\outputDir mod _ -> Path.concat [ outputDir, mod, "index.js" ])
      (\_ _ modName -> Path.concat [ "snapshots", "Snapshot", modName <> "-index.js" ])

copyFile :: String -> (String -> String -> String -> String) -> (String -> String -> String -> String) -> Aff Unit
copyFile outputDir toJsFile toOutFile = do
  mods <- FSA.readdir outputDir
  for_ mods \mod ->
    for_ (String.stripPrefix (Pattern "Snapshot.") mod) \modName -> do
      let
        jsFile = toJsFile outputDir mod modName
        snapshotFile = toOutFile outputDir mod modName
      whenM (liftEffect $ FS.exists snapshotFile) do
        FSA.unlink snapshotFile
      FSA.copyFile jsFile snapshotFile
