{-# LANGUAGE GADTs #-}

module Copilot.Compile.Rust.Compile
  ( compile
  ) where

import Copilot.Core ( Expr (..), Spec (..), Stream (..), Struct (..),
                      Trigger (..), Type (..), UExpr (..), UType (..),
                      Value (..) )
-- import qualified Core.Spec as Copilot
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as Rust
import Copilot.Compile.Rust.Name
import Copilot.Compile.Rust.Type
import Copilot.Compile.Rust.Expr
import Copilot.Compile.Rust.External
import Copilot.Compile.Rust.CodeGen
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Pretty as Rust
import Language.Rust.Data.Ident (mkIdent)
import qualified Control.Exception.Base as Copilot.Compile.Rust
-- import Copilot.Compile.Rust.Name (generatorOutputArgName)

import System.FilePath
import System.IO

compile :: String -> String -> Spec -> IO ()
compile dir name spec = do
    writeFile (dir </> name ++ ".rs") $ rustHeader ++ rustCode ++ "\n\n" ++ rustFooter
    where
      rustHeader = unlines
        [ "use std::marker::PhantomData;"
        , ""
        ]
      rustCode = show $ Rust.pretty' $ compileRs name spec
      rustFooter = unlines
        [ "pub struct Monitor<'a, T: MonitorTriggers> {"
        , "    state: MonitorState,"
        , "    phantom : PhantomData<&'a T>"
        , "}"
        , ""
        , "impl<T: MonitorTriggers> Monitor<'_, T> {"
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

compileRs :: String -> Spec -> Rust.SourceFile ()
compileRs name spec = Rust.SourceFile (Just name) attributes items
  where
    attributes = []
    items = [trait, inputStruct, stateStruct, stateStructDefault] ++ accessDeclarations ++ generatorFuncs ++ triggerFuncs ++ [stepFunction]
    trait = mkTriggerTrait triggers

    triggers = specTriggers spec
    streams = specStreams spec
    inputStruct = mkInputStruct (gatherExts streams triggers)
    stateStruct = mkStateStruct streams
    stateStructDefault = mkStateStructDefault streams
    accessDeclarations = map Copilot.Compile.Rust.CodeGen.mkAccessDeclnR streams
    -- monitorStruct = mkMonitorStruct
    stepFunction = Copilot.Compile.Rust.CodeGen.mkStep spec
    triggerFuncs = Copilot.Compile.Rust.CodeGen.translateTriggers triggers
    generatorFuncs = Copilot.Compile.Rust.CodeGen.mkGenerators streams
