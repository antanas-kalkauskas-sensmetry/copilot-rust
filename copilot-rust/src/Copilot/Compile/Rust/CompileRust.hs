{-# LANGUAGE GADTs #-}

module Copilot.Compile.Rust.CompileRust
  ( translateTriggers
  , mkStep
  , mkGenerators
  , mkAccessDeclnR
  ) where

import Copilot.Core ( Expr (..), Spec (..), Stream (..), Struct (..),
                      Trigger (..), Type (..), UExpr (..), UType (..),
                      Value (..) )
-- import qualified Core.Spec as Copilot
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as Rust
import Copilot.Compile.Rust.Type
import Copilot.Compile.Rust.ExprRust
import Copilot.Compile.Rust.Name (generatorName, streamName, indexName, streamAccessorName)
import qualified Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident (mkIdent)
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

mkRefType :: String -> Rust.Ty ()
mkRefType x = Rust.Rptr Nothing Rust.Immutable (mkType x) ()

mkMutableRefType :: String -> Rust.Ty ()
mkMutableRefType x = Rust.Rptr Nothing Rust.Mutable (mkType x) ()

mkImmutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent ident) Nothing ())) ty ()

mkMutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkMutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Mutable) (Rust.mkIdent ident) Nothing ())) ty ()

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
        (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorInput") "input", mkImmutableArg (mkRefType "MonitorState") "state"] (Just $ mkType "bool") False ())
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
        (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorInput") "input", mkImmutableArg (mkRefType "MonitorState") "state"] (Just $ transTypeR ty) False ())
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

mkPathExpr :: [String] -> Rust.Expr ()
mkPathExpr xs = Rust.PathExpr [] Nothing (Rust.Path False (map (\x -> Rust.PathSegment (Rust.mkIdent x) Nothing ()) xs) ()) ()

mkFieldAccessExpr :: Rust.Expr () -> String -> Rust.Expr ()
mkFieldAccessExpr struct field = Rust.FieldAccess [] struct (Rust.mkIdent field) ()

mkRef :: Rust.Expr () -> Rust.Expr ()
mkRef e = Rust.AddrOf [] Rust.Immutable e ()

mkTriggerRunnerExpr :: Trigger -> Rust.Expr ()
mkTriggerRunnerExpr trigger@(Trigger name _ _) =
    Rust.If
        []
        triggerGuardCallExpr
        (Rust.Block [Rust.Semi triggerCallExpr ()] Rust.Normal ())
        Nothing
        ()
    where
        triggerGuardCallExpr = Rust.Call [] (mkPathExpr [mkTriggerGuardName trigger]) [mkPathExpr ["input"], mkPathExpr ["state"]] ()
        triggerCallExpr = Rust.Call [] (mkFieldAccessExpr (mkPathExpr ["triggers"]) name) triggerCallArgs ()
        triggerCallArgs = map (\x -> Rust.Call [] (mkPathExpr [x]) [mkPathExpr ["input"], mkPathExpr ["state"]] ()) (mkTriggerArgNames trigger)


mkVariableReference :: String -> Rust.Expr ()
mkVariableReference ident = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent ident) Nothing ()] ()) ()

mkStep :: Spec -> Rust.Item ()
mkStep spec@(Spec _ _ triggers _) = 
    Rust.Fn
        []
        Rust.InheritedV
        (Rust.mkIdent "internal_step")
        (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorInput") "input", mkImmutableArg (mkMutableRefType "MonitorState") "state", mkImmutableArg (mkMutableRefType "T") "triggers"] (Just $ Rust.TupTy [] ()) False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [Rust.TyParam [] (Rust.mkIdent "T") [Rust.TraitTyParamBound (Rust.PolyTraitRef [] (Rust.TraitRef (Rust.Path False [Rust.PathSegment (Rust.mkIdent "MonitorTriggers") Nothing ()] ())) ()) Rust.None ()] Nothing ()] (Rust.WhereClause [] ()) ())
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
                tmpExpr = Rust.Call [] (mkVariableReference $ generatorName sId) [mkVariableReference "input", mkVariableReference "state"] ()

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


-- | Define an accessor functions for the ring buffer associated with a stream.
mkAccessDeclnR :: Stream -> Rust.Item ()
mkAccessDeclnR (Stream sId buff _ ty) =
  Rust.Fn
    []
    Rust.InheritedV
    (Rust.mkIdent name)
    (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorState") "state", mkImmutableArg (mkType "usize") "index"]
    (Just rustTy) False ())
    Rust.Normal
    Rust.NotConst
    Rust.Rust
    (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
    (Rust.Block
      [Rust.NoSemi expr ()]
      Rust.Normal
      ())
    ()
  
  -- C.FunDef cTy name params [] [C.Return (Just expr)]
  where
    streamIndex = Rust.FieldAccess [] (mkVariableReference "state") (mkIdent (indexName sId)) ()
    indexSum = Rust.Binary [] Rust.AddOp (mkVariableReference "index") streamIndex ()
    index      = Rust.Binary [] Rust.RemOp indexSum (Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral $ length buff) Rust.Unsuffixed () ) ()) ()
    expr = Rust.Index [] (Rust.FieldAccess [] (mkVariableReference "state") (mkIdent (streamName sId)) ()) index ()
    rustTy        = transTypeR ty
    name       = streamAccessorName sId