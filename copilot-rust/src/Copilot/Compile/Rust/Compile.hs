{-# LANGUAGE GADTs #-}

module Copilot.Compile.Rust.Compile
  ( compile
  , compileWith
  ) where

import Copilot.Core ( Expr (..), Spec (..), Stream (..), Struct (..),
                      Trigger (..), Type (..), UExpr (..), UType (..),
                      Value (..) )
-- import qualified Core.Spec as Copilot
import qualified Language.Rust.Syntax as Rust
import Copilot.Compile.Rust.Name ()
import Copilot.Compile.Rust.Type ()
import Copilot.Compile.Rust.Expr ()
import Copilot.Compile.Rust.External
import Copilot.Compile.Rust.CodeGen
import qualified Language.Rust.Pretty as Rust

import System.FilePath
import Copilot.Compile.Rust.Settings (mkDefaultRustSettings, RustSettings (rustMonitorStructName, rustSettingsOutputDirectory))
import System.Directory (createDirectoryIfMissing)


compile :: String -> Spec -> IO ()
compile = compileWith mkDefaultRustSettings

compileWith :: RustSettings -> String -> Spec -> IO ()
compileWith rustSettings name spec = do
    let dir = rustSettingsOutputDirectory rustSettings
    createDirectoryIfMissing True dir
    writeFile (dir </> name ++ ".rs") $ rustCode ++ "\n\n" ++ rustFooter
    where
      rustCode = show $ Rust.pretty' $ compileRs name spec
      rustFooter = unlines
        [ "pub struct " ++ structName ++ "<'a, T: MonitorTriggers> {"
        , "    state: MonitorState,"
        , "    phantom : PhantomData<&'a T>"
        , "}"
        , ""
        , "impl<T: MonitorTriggers> " ++ structName ++ "<'_, T> {"
        , "    pub fn new() -> Self {"
        , "        Monitor {"
        , "            state: MonitorState::default(),"
        , "            phantom: PhantomData"
        , "        }"
        , "    }"
        , ""
        , "    pub fn step(&mut self, input: &MonitorInput, triggers: &mut T) {"
        , "        internal_step(input, &mut self.state, triggers);"
        , "    }"
        , "}"
        ]
      structName = rustMonitorStructName rustSettings

compileRs :: String -> Spec -> Rust.SourceFile ()
compileRs name spec = Rust.SourceFile (Just name) attributes items
  where
    attributes = []
    items =
      [usePhantomDataItem, trait, inputStruct, stateStruct, stateStructDefault] 
      ++ accessDeclarations
      ++ generatorFuncs
      ++ triggerFuncs
      ++ [stepFunction]

    trait = mkTriggerTrait triggers
    usePhantomDataItem = Rust.Use [] Rust.InheritedV (Rust.UseTreeSimple (mkPath ["std", "marker", "PhantomData"]) Nothing ()) ()
    triggers = specTriggers spec
    streams = specStreams spec
    inputStruct = mkInputStruct (gatherExts streams triggers)
    stateStruct = mkStateStruct streams
    stateStructDefault = mkStateStructDefault streams
    accessDeclarations = map mkAccessDecln streams
    stepFunction = mkStep spec
    triggerFuncs = translateTriggers triggers
    generatorFuncs = mkGenerators streams
