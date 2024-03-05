module Copilot.Compile.Rust.CompileRust
  ( translateTriggers
  , mkStep
  ) where

import qualified Copilot.Core as Copilot
import qualified Copilot.Core.Spec as Copilot
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as Rust
import Copilot.Compile.Rust.ExprRust
import Copilot.Compile.Rust.Type

translateTriggers :: [Copilot.Trigger] -> [Rust.Item ()]
translateTriggers = concatMap translateTrigger

translateTrigger :: Copilot.Trigger -> [Rust.Item ()]
translateTrigger trigger@(Copilot.Trigger _ _ args) = guardFn : triggerFns
    where
        guardFn = mkTriggerGuardFn trigger
        triggerFns = zipWith mkTriggerArgFn (mkTriggerArgNames trigger) args

mkType :: String -> Rust.Ty ()
mkType x = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent x) Nothing ()] ()) ()

mkImmutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent ident) Nothing ())) ty ()

mkImmutableRefArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableRefArg ty ident = Rust.Arg (Just (Rust.RefP (Rust.IdentP (Rust.ByValue Rust.Immutable) (Rust.mkIdent ident) Nothing ()) Rust.Immutable ())) ty ()

mkMutableRefArg :: Rust.Ty () -> String -> Rust.Arg ()
mkMutableRefArg ty ident = Rust.Arg (Just (Rust.RefP (Rust.IdentP (Rust.ByValue Rust.Mutable) (Rust.mkIdent ident) Nothing ()) Rust.Immutable ())) ty ()

mkTriggerGuardName :: Copilot.Trigger -> String
mkTriggerGuardName (Copilot.Trigger name _ _) = name ++ "_guard"

mkTriggerArgNames :: Copilot.Trigger -> [String]
mkTriggerArgNames (Copilot.Trigger name _ args) =
    zipWith
        (++)
        (replicate (length args) name)
        (map (\x -> "_arg" ++ show x) [0 .. length args])

mkTriggerGuardFn :: Copilot.Trigger -> Rust.Item ()
mkTriggerGuardFn trigger@(Copilot.Trigger _ guard _) =
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

mkTriggerArgFn :: String -> Copilot.UExpr -> Rust.Item ()
mkTriggerArgFn name (Copilot.UExpr ty e) = 
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

mkTriggerRunnerExpr :: Copilot.Trigger -> Rust.Expr ()
mkTriggerRunnerExpr trigger@(Copilot.Trigger name _ _) =
    Rust.If
        []
        triggerGuardCallExpr
        (Rust.Block [Rust.Semi triggerCallExpr ()] Rust.Normal ())
        Nothing
        ()
    where
        triggerGuardCallExpr = Rust.Call [] (Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent $ mkTriggerGuardName trigger) Nothing ()] ()) ()) [] ()
        triggerCallExpr = Rust.Call [] (Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent name) Nothing ()] ()) ()) triggerCallArgs ()
        triggerCallArgs = map (\x -> Rust.Call [] (Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent x) Nothing ()] ()) ()) [] ()) (mkTriggerArgNames trigger)

mkStep :: Copilot.Spec -> Rust.Item ()
mkStep (Copilot.Spec _ _ triggers _) = 
    Rust.Fn
        []
        Rust.InheritedV
        (Rust.mkIdent "internalStep")
        (Rust.FnDecl [mkMutableRefArg (mkType "MonitorInput") "input", mkMutableRefArg (mkType "MonitorState") "state"] (Just $ mkType "whatever") False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
        (Rust.Block (map (\x -> Rust.Semi (mkTriggerRunnerExpr x) ()) triggers) Rust.Normal ())
        ()
