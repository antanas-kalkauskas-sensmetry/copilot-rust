{-# LANGUAGE GADTs #-}

-- | High-level translation of Copilot Core into Rust.
module Copilot.Compile.Rust.CodeGen
    ( mkTriggerTrait
    , mkInputStruct
    , mkStateStruct
    , mkStateStructDefault
    , mkGenerators
    , translateTriggers
    , mkStep
    , mkAccessDecln
    , mkPath
    )
  where

-- External imports
import qualified Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident

-- Internal imports: Copilot
import Copilot.Core ( Expr (..), Id, Stream (..), Struct (..), Trigger (..),
                      Type (..), UExpr (..), Value (..), fieldName, typeSize, Spec (..) )

-- Internal imports
import Copilot.Compile.Rust.Expr     ( transExpr, constArray )
import Copilot.Compile.Rust.External ( External (..) )
import Copilot.Compile.Rust.Name     ( argNames, argTempNames, generatorName,
                                      guardName, indexName, streamAccessorName,
                                      streamName )
import Copilot.Compile.Rust.Type     ( transType )

-- * Rust code generation functions

mkTriggerTraitHelper :: Trigger -> Rust.TraitItem ()
mkTriggerTraitHelper trigger =
  Rust.MethodT
    []
    (mkIdent (triggerName trigger))
    (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
    (Rust.MethodSig Rust.Normal Rust.NotConst Rust.Rust
                (Rust.FnDecl (Rust.SelfRegion Nothing Rust.Mutable () : arguments)
                        Nothing
                        False ())) 
    Nothing
    ()
  where
    arguments = map mkArgument argsAux
    mkArgument (triggerArgName, ty) = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Immutable) (mkIdent triggerArgName) Nothing ())) ty ()
    argsAux = zip (argNames (triggerName trigger)) (map tempType (triggerArgs trigger))

    tempType :: UExpr -> Rust.Ty ()
    tempType (UExpr { uExprType = ty }) = transType ty

mkTriggerTrait :: [Trigger] -> Rust.Item ()
mkTriggerTrait xs =
  Rust.Trait
    []
    Rust.PublicV
    (mkIdent "MonitorTriggers")
    False Rust.Normal
    (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
    []
    (map mkTriggerTraitHelper xs)
    ()


mkInputStructField :: External -> Rust.StructField ()
mkInputStructField External{ extName = name, extType = ty} =
  Rust.StructField (Just (mkIdent name)) Rust.PublicV (transType ty) [] ()

mkInputStruct :: [External] -> Rust.Item ()
mkInputStruct xs = Rust.StructItem
  []
  Rust.PublicV
  (mkIdent "MonitorInput")
  (Rust.StructD
    (map mkInputStructField xs)
    ()
    )
  (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
  ()


mkBuffDecln :: Stream -> Rust.StructField ()
mkBuffDecln (Stream sId buff _ ty) = 
  Rust.StructField (Just (mkIdent (streamName sId))) Rust.InheritedV (Rust.Array (transType ty) (Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral $ length buff) Rust.Unsuffixed ()) ()) ()) [] ()

mkIndexDecln :: Stream -> Rust.StructField ()
mkIndexDecln (Stream sId buff _ ty) = 
  Rust.StructField (Just (mkIdent (streamName sId ++ "_idx"))) Rust.InheritedV ( Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (mkIdent "usize") Nothing ()] ()) ()) [] ()

mkStateStruct :: [Stream] -> Rust.Item ()
mkStateStruct streams = 
  Rust.StructItem
    []
    Rust.InheritedV
    (mkIdent "MonitorState")
    (Rust.StructD
      fields
      ()
      )
    (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
    ()
  where
    fields = map mkBuffDecln streams ++ map mkIndexDecln streams

mkStateStructDefault :: [Stream] -> Rust.Item ()
mkStateStructDefault streams =
  Rust.Impl []
  Rust.InheritedV
  Rust.Final
  Rust.Normal
  Rust.Positive
  (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
  (Just (Rust.TraitRef (Rust.Path False [Rust.PathSegment (mkIdent "Default") Nothing ()] ())))
  (Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (mkIdent "MonitorState") Nothing ()] ()) ())
  [defaultFun]
  ()
  where
    defaultFun = Rust.MethodI
      []
      Rust.InheritedV
      Rust.Final
      (mkIdent "default")
      (Rust.Generics [] [] (Rust.WhereClause [] ()) ())
      (Rust.MethodSig Rust.Normal Rust.NotConst Rust.Rust (Rust.FnDecl [] (Just (Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (mkIdent "Self") Nothing ()] ()) ())) False ()))
      (Rust.Block [
        Rust.NoSemi
        (Rust.Struct [] (Rust.Path False [Rust.PathSegment (mkIdent "Self") Nothing ()] ())
        (initIndices++initBuffers)
        Nothing
        ())
        ()
      ]
      Rust.Normal
      ()
      )
      ()
    initIndices = map mkInitIndexField streams
    initBuffers = map mkInitBufferField streams

    mkInitIndexField (Stream sId _ _ _) = Rust.Field (mkIdent (indexName sId )) (Just $ Rust.Lit [] (Rust.Int Rust.Dec 0 Rust.Unsuffixed ()) ()) ()
    mkInitBufferField (Stream sId buff _ ty) = Rust.Field (mkIdent (streamName sId)) (Just $ constArray ty buff) ()



mkVariableReference :: String -> Rust.Expr ()
mkVariableReference ident = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (mkIdent ident) Nothing ()] ()) ()

mkType :: String -> Rust.Ty ()
mkType x = Rust.PathTy Nothing (Rust.Path False [Rust.PathSegment (mkIdent x) Nothing ()] ()) ()

mkImmutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Immutable) (mkIdent ident) Nothing ())) ty ()

mkImmutableRefArg :: Rust.Ty () -> String -> Rust.Arg ()
mkImmutableRefArg ty ident = Rust.Arg (Just (Rust.RefP (Rust.IdentP (Rust.ByValue Rust.Immutable) (mkIdent ident) Nothing ()) Rust.Immutable ())) ty ()

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

mkRefType :: String -> Rust.Ty ()
mkRefType x = Rust.Rptr Nothing Rust.Immutable (mkType x) ()

mkMutableRefType :: String -> Rust.Ty ()
mkMutableRefType x = Rust.Rptr Nothing Rust.Mutable (mkType x) ()

mkMutableArg :: Rust.Ty () -> String -> Rust.Arg ()
mkMutableArg ty ident = Rust.Arg (Just (Rust.IdentP (Rust.ByValue Rust.Mutable) (mkIdent ident) Nothing ())) ty ()

mkTriggerGuardName :: Trigger -> String
mkTriggerGuardName (Trigger name _ _) = guardName name

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
        (mkIdent $ mkTriggerGuardName trigger)
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
        (mkIdent name)
        (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorInput") "input", mkImmutableArg (mkRefType "MonitorState") "state"] (Just $ transType ty) False ())
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

mkPath :: [String] -> Rust.Path ()
mkPath xs = Rust.Path False (map (\x -> Rust.PathSegment (mkIdent x) Nothing ()) xs) ()

mkPathExpr :: [String] -> Rust.Expr ()
mkPathExpr xs = Rust.PathExpr [] Nothing (mkPath xs) ()

mkFieldAccessExpr :: Rust.Expr () -> String -> Rust.Expr ()
mkFieldAccessExpr struct field = Rust.FieldAccess [] struct (mkIdent field) ()

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
        triggerGuardCallExpr = Rust.Call [] (mkPathExpr [guardName name]) [mkPathExpr ["input"], mkPathExpr ["state"]] ()
        triggerCallExpr = Rust.Call [] (mkFieldAccessExpr (mkPathExpr ["triggers"]) name) triggerCallArgs ()
        triggerCallArgs = map (\x -> Rust.Call [] (mkPathExpr [x]) [mkPathExpr ["input"], mkPathExpr ["state"]] ()) (mkTriggerArgNames trigger)

mkStep :: Spec -> Rust.Item ()
mkStep spec@(Spec _ _ triggers _) = 
    Rust.Fn
        []
        Rust.InheritedV
        (mkIdent "internal_step")
        (Rust.FnDecl [mkImmutableArg (mkRefType "MonitorInput") "input", mkImmutableArg (mkMutableRefType "MonitorState") "state", mkImmutableArg (mkMutableRefType "T") "triggers"] (Just $ Rust.TupTy [] ()) False ())
        Rust.Normal
        Rust.NotConst
        Rust.Rust
        (Rust.Generics [] [Rust.TyParam [] (mkIdent "T") [Rust.TraitTyParamBound (Rust.PolyTraitRef [] (Rust.TraitRef (Rust.Path False [Rust.PathSegment (mkIdent "MonitorTriggers") Nothing ()] ())) ()) Rust.None ()] Nothing ()] (Rust.WhereClause [] ()) ())
        (Rust.Block (map (\x -> Rust.Semi (mkTriggerRunnerExpr x) ()) triggers ++ temps ++ buffUpdates ++ indexUpdates) Rust.Normal ())
        ()
    where
        (temps, buffUpdates, indexUpdates) = unzip3 (map mkUpdateGlobals streams)
        streams = specStreams spec
        mkUpdateGlobals :: Stream -> (Rust.Stmt (), Rust.Stmt (), Rust.Stmt ())
        mkUpdateGlobals (Stream sId buff _expr ty) =
            (tmpDcln, bufferUpdate, indexUpdate)
                where
                tmpDcln = Rust.Local (Rust.IdentP (Rust.ByValue Rust.Immutable) (mkIdent tmpVar) Nothing ()) (Just rustTy) (Just tmpExpr) [] ()
                tmpExpr = Rust.Call [] (mkVariableReference $ generatorName sId) [mkVariableReference "input", mkVariableReference "state"] ()

                bufferUpdate = Rust.Semi
                    (Rust.Assign [] bufferVar (mkVariableReference tmpVar) ()) ()

                indexUpdate = Rust.Semi
                    (Rust.Assign [] indexVar newIndex ()) ()

                tmpVar   = streamName sId ++ "_tmp"
                bufferVar = Rust.Index [] (Rust.FieldAccess [] (mkVariableReference "state") (mkIdent $ streamName sId) ()) indexVar ()
                indexVar = Rust.FieldAccess [] (mkVariableReference "state") (mkIdent (indexName sId)) ()
                incrementedIndex = Rust.Binary [] Rust.AddOp indexVar (Rust.Lit [] (Rust.Int Rust.Dec 1 Rust.Unsuffixed ()) ()) ()
                newIndex = Rust.Binary [] Rust.RemOp incrementedIndex (Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral $ length buff) Rust.Unsuffixed ()) ()) ()
                rustTy      = transType ty


-- | Define an accessor functions for the ring buffer associated with a stream.
mkAccessDecln :: Stream -> Rust.Item ()
mkAccessDecln (Stream sId buff _ ty) =
  Rust.Fn
    []
    Rust.InheritedV
    (mkIdent name)
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

  where
    streamIndex = Rust.FieldAccess [] (mkVariableReference "state") (mkIdent (indexName sId)) ()
    indexSum = Rust.Binary [] Rust.AddOp (mkVariableReference "index") streamIndex ()
    index      = Rust.Binary [] Rust.RemOp indexSum (Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral $ length buff) Rust.Unsuffixed () ) ()) ()
    expr = Rust.Index [] (Rust.FieldAccess [] (mkVariableReference "state") (mkIdent (streamName sId)) ()) index ()
    rustTy        = transType ty
    name       = streamAccessorName sId
