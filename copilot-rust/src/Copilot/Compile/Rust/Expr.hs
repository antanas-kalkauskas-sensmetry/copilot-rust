{-# LANGUAGE GADTs #-}

module Copilot.Compile.Rust.Expr
    ( transExpr
    , constArray
    )
  where

-- External imports

-- Internal imports: Copilot
import Copilot.Core ( Expr (..), Field (..), Op1 (..), Op2 (..), Op3 (..),
                      Type (..), Value (..), accessorName, arrayElems,
                      toValues )

-- Internal imports
import Copilot.Compile.Rust.Name
import Copilot.Compile.Rust.Type (transLocalVarDeclType, transType)

-- import qualified Core as Copilot
import GHC.Float (float2Double)
import qualified Language.Rust.Data.Ident as Rust
import qualified Language.Rust.Syntax as Rust

transExpr :: Expr a -> Rust.Expr ()
-- Const expressions
transExpr (Const ty x) = constTy ty x
transExpr (Drop _ amount sId) =
    Rust.Call [] functionName [stateVar, index] ()
  where
    accessVar = streamAccessorName sId
    functionName = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent accessVar) Nothing ()] ()) ()
    index = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral amount) Rust.Unsuffixed ()) ()
    stateVar = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent "state") Nothing ()] ()) ()

-- Local expressions
transExpr (Local{}) = error "not supported" -- TODO

-- Var expressions
transExpr (Var _ name) = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent name) Nothing ()] ()) ()

-- ExternVar expressions
transExpr (ExternVar _ name _) = Rust.FieldAccess [] inputStruct (Rust.mkIdent name) ()
  where
    inputStruct = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent "input") Nothing ()] ()) ()

-- Op1 expressions
transExpr (Op1 Not x) = Rust.Unary [] Rust.Not (transExpr x) ()
transExpr (Op1 (Abs _) _) = error "not supported" -- TODO
transExpr (Op1 (Sign _) _) = error "not supported" -- TODO
transExpr (Op1 (Recip _) x) = Rust.Binary [] Rust.DivOp (translateIntLiteral (1 :: Integer)) (transExpr x) ()
transExpr (Op1 (Exp _) _) = error "not supported" -- TODO
transExpr (Op1 (Sqrt _) _) = error "not supported" -- TODO
transExpr (Op1 (Log _) _) = error "not supported" -- TODO
transExpr (Op1 (Sin _) _) = error "not supported" -- TODO
transExpr (Op1 (Tan _) _) = error "not supported" -- TODO
transExpr (Op1 (Cos _) _) = error "not supported" -- TODO
transExpr (Op1 (Asin _) _) = error "not supported" -- TODO
transExpr (Op1 (Atan _) _) = error "not supported" -- TODO
transExpr (Op1 (Acos _) _) = error "not supported" -- TODO
transExpr (Op1 (Sinh _) _) = error "not supported" -- TODO
transExpr (Op1 (Tanh _) _) = error "not supported" -- TODO
transExpr (Op1 (Cosh _) _) = error "not supported" -- TODO
transExpr (Op1 (Asinh _) _) = error "not supported" -- TODO
transExpr (Op1 (Atanh _) _) = error "not supported" -- TODO
transExpr (Op1 (Acosh _) _) = error "not supported" -- TODO
transExpr (Op1 (Ceiling _) _) = error "not supported" -- TODO
transExpr (Op1 (Floor _) _) = error "not supported" -- TODO
transExpr (Op1 (BwNot _) _) = error "not supported" -- TODO
transExpr (Op1 (Cast ty1 ty2) expr) = Rust.Cast [] (transExpr expr) (transType ty2) ()
transExpr (Op1 (GetField{}) _) = error "not supported" -- TODO

-- Op2 expressions
transExpr (Op2 And x y) = translateBinaryOp x y Rust.AndOp
transExpr (Op2 Or x y) = translateBinaryOp x y Rust.OrOp
transExpr (Op2 (Add _) x y) = translateBinaryOp x y Rust.AddOp
transExpr (Op2 (Sub _) x y) = translateBinaryOp x y Rust.SubOp
transExpr (Op2 (Mul _) x y) = translateBinaryOp x y Rust.MulOp
transExpr (Op2 (Mod _) x y) = translateBinaryOp x y Rust.RemOp
transExpr (Op2 (Div _) x y) = translateBinaryOp x y Rust.DivOp
transExpr (Op2 (Fdiv _) x y) = translateBinaryOp x y Rust.DivOp
transExpr (Op2 (Pow _) _ _) = error "not supported" -- TODO
transExpr (Op2 (Logb _) _ _) = error "not supported" -- TODO
transExpr (Op2 (Atan2 _) _ _) = error "not supported" -- TODO
transExpr (Op2 (Eq _) x y) = translateBinaryOp x y Rust.EqOp
transExpr (Op2 (Ne _) x y) = translateBinaryOp x y Rust.NeOp
transExpr (Op2 (Le _) x y) = translateBinaryOp x y Rust.LeOp
transExpr (Op2 (Ge _) x y) = translateBinaryOp x y Rust.GeOp
transExpr (Op2 (Lt _) x y) = translateBinaryOp x y Rust.LtOp
transExpr (Op2 (Gt _) x y) = translateBinaryOp x y Rust.GtOp
transExpr (Op2 (BwAnd _) x y) = translateBinaryOp x y Rust.BitAndOp
transExpr (Op2 (BwOr _) x y) = translateBinaryOp x y Rust.BitOrOp
transExpr (Op2 (BwXor _) x y) = translateBinaryOp x y Rust.BitXorOp
transExpr (Op2 (BwShiftL _ _) x y) = translateBinaryOp x y Rust.ShlOp
transExpr (Op2 (BwShiftR _ _) x y) = translateBinaryOp x y Rust.ShrOp
transExpr (Op2 (Index _) x y) = Rust.Index [] (transExpr x) (transExpr y) ()

-- Op3 expressions
transExpr (Op3 (Mux _) predicate consequent alternative) =
    Rust.If
        []
        (transExpr predicate)
        (Rust.Block [Rust.NoSemi (transExpr consequent) ()] Rust.Normal ())
        (Just (Rust.BlockExpr [] (Rust.Block [Rust.NoSemi (transExpr alternative) ()] Rust.Normal ()) ()))
        ()
-- Label expressions
transExpr (Label _ _ e) = transExpr e

{- | Transform a Copilot Core literal, based on its value and type, into a Rust
 literal.
-}
constTy :: Type a -> a -> Rust.Expr ()
constTy Bool x = Rust.Lit [] (Rust.Bool x Rust.Unsuffixed ()) ()
constTy Int8 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.I8 ()) ()
constTy Int16 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.I16 ()) ()
constTy Int32 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.I32 ()) ()
constTy Int64 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.I64 ()) ()
constTy Word8 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.U8 ()) ()
constTy Word16 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.U16 ()) ()
constTy Word32 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.U32 ()) ()
constTy Word64 x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.U64 ()) ()
constTy Float x = Rust.Lit [] (Rust.Float (float2Double x) Rust.F32 ()) ()
constTy Double x = Rust.Lit [] (Rust.Float x Rust.F64 ()) ()
constTy (Struct _) _ = error "not supported"
constTy (Array _) _ = error "not supported"

-- | Transform a Copilot Array, based on the element values and their type,
-- into a list of Rust initializer values.
constArray :: Type a -> [a] -> Rust.Expr ()
constArray ty xs =
    Rust.Vec
        []
        (map (constTy ty) xs)
        ()

-- Helpers
translateIntLiteral :: Integral a => a -> Rust.Expr ()
translateIntLiteral x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.Unsuffixed ()) ()

translateBinaryOp :: Expr a -> Expr b -> Rust.BinOp -> Rust.Expr ()
translateBinaryOp x y op = Rust.Binary [] op (transExpr x) (transExpr y) ()
