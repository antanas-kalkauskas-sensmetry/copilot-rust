{-# LANGUAGE GADTs #-}

module Copilot.Compile.Rust.CompileRust
  ( translateTriggers
  , mkStep
  , mkGenerators
  ) where

import Copilot.Core ( Expr (..), Spec (..), Stream (..), Struct (..),
                      Trigger (..), Type (..), UExpr (..), UType (..),
                      Value (..) )
-- import qualified Core.Spec as Copilot
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as Rust
import Copilot.Compile.Rust.Type
import Copilot.Compile.Rust.ExprRust
import Copilot.Compile.Rust.Name (generatorName, streamName, indexName)
-- import Copilot.Compile.Rust.Name (generatorOutputArgName)

translateTriggers :: [Trigger] -> [Rust.Item ()]
translateTriggers = concatMap translateTrigger

translateTrigger :: Trigger -> [Rust.Item ()]
translateTrigger trigger@(Trigger _ _ args) = guardFn : triggerFns
    where
        guardFn = mkTriggerGuardFn trigger
        triggerFns = zipWith mkTriggerArgFn (mkTriggerArgNames trigger) args

mkGenerators :: [Stream] -> [Rust.Item ()]
mkGenerators = concatMap mkGenerator

mkGenerator :: Stream -> [Rust.Item ()]
mkGenerator (Stream sId _ expr ty) = [genFun]
    where
        genFun = mkTriggerArgFn (generatorName sId) (UExpr ty expr)

mkType :: String -> Rust.Ty ()
mkType x = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent x) Nothing ()] ()) ()

mkImmutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent ident) Nothing ())) ty ()

mkImmutableRefArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableRefArg ty ident = Rust.Arg (Just (Rust.RefP (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent ident) Nothing ()) Rust.Immutable ())) ty ()

mkMutableRefArg :: Rust.Ty () -> String -> Rust.Arg ()
mkMutableRefArg ty ident = Rust.Arg (Just (Rust.RefP (Rust.IdentP (Rust.ByValue Rust.Mutable) (Rust.mkIdent ident) Nothing ()) Rust.Immutable ())) ty ()

mkTriggerGuardName :: Trigger -> String
mkTriggerGuardName (Trigger name _ _) = name ++ "_guard"

mkTriggerArgNames :: Trigger -> [String]
mkTriggerArgNames (Trigger name _ args) =
    zipWith
        (++)
        (replicate (length args) name)
        (map (\x -> "_arg" ++ show x) [0 .. length args])

mkTriggerGuardFn :: Trigger -> Rust.Item ()
mkTriggerGuardFn trigger@(Trigger _ guard _) =
    Rust.Fn
        []
        Rust.InheritedV
        (Rust.mkIdent $ mkTriggerGuardName trigger)
        (Rust.FnDecl [mkImmutableRefArg (mkType "MonitorInput") "input", mkImmutableRefArg (mkType "MonitorState") "state"] (Just $ mkType "bool") False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
        (mkReturnBlock $ transExpr guard)
        ()

mkTriggerArgFn :: String -> UExpr -> Rust.Item ()
mkTriggerArgFn name (UExpr ty e) = 
    Rust.Fn
        []
        Rust.InheritedV
        (Rust.mkIdent name)
        (Rust.FnDecl [mkImmutableRefArg (mkType "MonitorInput") "input", mkImmutableRefArg (mkType "MonitorState") "state"] (Just $ transTypeR ty) False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
        (mkReturnBlock $ transExpr e)
        ()

mkReturnBlock :: Rust.Expr () -> Rust.Block ()
mkReturnBlock e = Rust.Block [ Rust.Semi (Rust.Ret [] (Just e) ()) () ] Rust.Normal ()

mkIf :: Rust.Expr () -> Rust.Block () -> Rust.Expr ()
mkIf predicate consequent = Rust.If [] predicate consequent Nothing ()

mkPathExpr :: String -> Rust.Expr ()
mkPathExpr x = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent x) Nothing ()] ()) ()

mkTriggerRunnerExpr :: Trigger -> Rust.Expr ()
mkTriggerRunnerExpr trigger@(Trigger name _ _) =
    Rust.If
        []
        triggerGuardCallExpr
        (Rust.Block [Rust.Semi triggerCallExpr ()] Rust.Normal ())
        Nothing
        ()
    where
        triggerGuardCallExpr = Rust.Call [] (mkPathExpr $ mkTriggerGuardName trigger) [] ()
        triggerCallExpr = Rust.Call [] (mkPathExpr name) triggerCallArgs ()
        triggerCallArgs = map (\x -> Rust.Call [] (mkPathExpr x) [mkPathExpr "input", mkPathExpr "state"] ()) (mkTriggerArgNames trigger)


mkVariableReference :: String -> Rust.Expr ()
mkVariableReference ident = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent ident) Nothing ()] ()) ()

mkStep :: Spec -> Rust.Item ()
mkStep spec@(Spec _ _ triggers _) = 
    Rust.Fn
        []
        Rust.InheritedV
        (Rust.mkIdent "internalStep")
        (Rust.FnDecl [mkImmutableRefArg (mkType "MonitorInput") "input", mkMutableRefArg (mkType "MonitorState") "state"] (Just $ mkType "whatever") False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
        (Rust.Block (map (\x -> Rust.Semi (mkTriggerRunnerExpr x) ()) triggers ++ temps ++ buffUpdates ++ indexUpdates) Rust.Normal ())
        ()
    where
        (temps, buffUpdates, indexUpdates) = unzip3 (map mkUpdateGlobalsR streams)
        streams = specStreams spec
        mkUpdateGlobalsR :: Stream -> (Rust.Stmt (), Rust.Stmt (), Rust.Stmt ())
        mkUpdateGlobalsR (Stream sId buff _expr ty) =
            (tmpDcln, bufferUpdate, indexUpdate)
                where
                tmpDcln = Rust.Local (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent tmpVar) Nothing ()) (Just rustTy) (Just tmpExpr) [] ()
                tmpExpr = Rust.Call [] (mkVariableReference $ generatorName sId) [] ()

                bufferUpdate = Rust.Semi
                    (Rust.Assign [] bufferVar (mkVariableReference tmpVar) ()) ()

                indexUpdate = Rust.Semi
                    (Rust.Assign [] indexVar newIndex ()) ()

                tmpVar   = streamName sId ++ "_tmp"
                bufferVar = Rust.Index [] (Rust.FieldAccess [] (mkVariableReference "state") (Rust.mkIdent $ streamName sId) ()) indexVar ()
                indexVar = Rust.FieldAccess [] (mkVariableReference "state") (Rust.mkIdent (indexName sId)) ()
                incrementedIndex = Rust.Binary [] Rust.AddOp indexVar (Rust.Lit [] (Rust.Int Rust.Dec 1 Rust.Unsuffixed ()) ()) ()
                newIndex = Rust.Binary [] Rust.RemOp incrementedIndex (Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral $ length buff) Rust.Unsuffixed ()) ()) ()
                -- val      = C.Funcall (C.Ident $ generatorName sId) []
                rustTy      = transTypeR ty
