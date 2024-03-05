{-# LANGUAGE GADTs #-}

import qualified Copilot.Core as Copilot
import GHC.Float (float2Double)
import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Data.Ident as Rust

transExpr :: Copilot.Expr a -> Rust.Expr ()

-- Const expressions
transExpr (Copilot.Const Copilot.Bool x) = Rust.Lit [] (Rust.Bool x Rust.Unsuffixed ()) ()
transExpr (Copilot.Const Copilot.Int8 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Int16 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Int32 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Int64 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Word8 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Word16 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Word32 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Word64 x) = translateIntLiteral x
transExpr (Copilot.Const Copilot.Float x) = Rust.Lit [] (Rust.Float (float2Double x) Rust.Unsuffixed ()) ()
transExpr (Copilot.Const Copilot.Double x) = Rust.Lit [] (Rust.Float x Rust.Unsuffixed ()) ()
transExpr (Copilot.Const (Copilot.Array _) _) = error "not supported" -- TODO
transExpr (Copilot.Const (Copilot.Struct _) _) = error "not supported" -- TODO

-- Drop expressions
transExpr (Copilot.Drop {}) = error "not supported" -- TODO

-- Local expressions
transExpr (Copilot.Local {}) = error "not supported" -- TODO

-- Var expressions
transExpr (Copilot.Var _ name) = Rust.PathExpr [] Nothing (Rust.Path False [Rust.PathSegment (Rust.mkIdent name) Nothing ()] ()) ()

-- ExternVar expressions
transExpr (Copilot.ExternVar {}) = error "not supported" -- TODO

-- Op1 expressions
transExpr (Copilot.Op1 Copilot.Not x) = Rust.Unary [] Rust.Not (transExpr x) ()
transExpr (Copilot.Op1 (Copilot.Abs _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Sign _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Recip _) x) = Rust.Binary [] Rust.DivOp (translateIntLiteral (1 :: Integer)) (transExpr x) ()
transExpr (Copilot.Op1 (Copilot.Exp _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Sqrt _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Log _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Sin _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Tan _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Cos _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Asin _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Atan _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Acos _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Sinh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Tanh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Cosh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Asinh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Atanh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Acosh _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Ceiling _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Floor _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.BwNot _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.Cast _ _) _) = error "not supported" -- TODO
transExpr (Copilot.Op1 (Copilot.GetField {}) _) = error "not supported" -- TODO

-- Op2 expressions
transExpr (Copilot.Op2 Copilot.And x y) = translateBinaryOp x y Rust.AndOp
transExpr (Copilot.Op2 Copilot.Or x y) = translateBinaryOp x y Rust.OrOp
transExpr (Copilot.Op2 (Copilot.Add _) x y) = translateBinaryOp x y Rust.AddOp
transExpr (Copilot.Op2 (Copilot.Sub _) x y) = translateBinaryOp x y Rust.SubOp
transExpr (Copilot.Op2 (Copilot.Mul _) x y) = translateBinaryOp x y Rust.MulOp
transExpr (Copilot.Op2 (Copilot.Mod _) x y) = translateBinaryOp x y Rust.RemOp
transExpr (Copilot.Op2 (Copilot.Div _) x y) = translateBinaryOp x y Rust.DivOp
transExpr (Copilot.Op2 (Copilot.Fdiv _) _ _) = error "not supported" -- TODO
transExpr (Copilot.Op2 (Copilot.Pow _) _ _) = error "not supported" -- TODO
transExpr (Copilot.Op2 (Copilot.Logb _) _ _) = error "not supported" -- TODO
transExpr (Copilot.Op2 (Copilot.Atan2 _) _ _) = error "not supported" -- TODO
transExpr (Copilot.Op2 (Copilot.Eq _) x y) = translateBinaryOp x y Rust.EqOp
transExpr (Copilot.Op2 (Copilot.Ne _) x y) = translateBinaryOp x y Rust.NeOp
transExpr (Copilot.Op2 (Copilot.Le _) x y) = translateBinaryOp x y Rust.LeOp
transExpr (Copilot.Op2 (Copilot.Ge _) x y) = translateBinaryOp x y Rust.GeOp
transExpr (Copilot.Op2 (Copilot.Lt _) x y) = translateBinaryOp x y Rust.LtOp
transExpr (Copilot.Op2 (Copilot.Gt _) x y) = translateBinaryOp x y Rust.GtOp
transExpr (Copilot.Op2 (Copilot.BwAnd _) x y) = translateBinaryOp x y Rust.BitAndOp
transExpr (Copilot.Op2 (Copilot.BwOr _) x y) = translateBinaryOp x y Rust.BitOrOp
transExpr (Copilot.Op2 (Copilot.BwXor _) x y) = translateBinaryOp x y Rust.BitXorOp
transExpr (Copilot.Op2 (Copilot.BwShiftL _ _) x y) = translateBinaryOp x y Rust.ShlOp
transExpr (Copilot.Op2 (Copilot.BwShiftR _ _) x y) = translateBinaryOp x y Rust.ShrOp
transExpr (Copilot.Op2 (Copilot.Index _) x y) = Rust.Index [] (transExpr x) (transExpr y) ()

-- Op3 expressions
transExpr (Copilot.Op3 (Copilot.Mux _) predicate consequent alternative) =
    Rust.If []
        (transExpr predicate)
        (Rust.Block [ Rust.NoSemi (transExpr consequent) () ] Rust.Normal ())
        (Just (Rust.BlockExpr [] (Rust.Block [ Rust.NoSemi (transExpr alternative) () ] Rust.Normal ()) ()))
        ()

-- Label expressions
transExpr (Copilot.Label _ _ e) = transExpr e

-- Helpers
translateIntLiteral :: Integral a => a -> Rust.Expr ()
translateIntLiteral x = Rust.Lit [] (Rust.Int Rust.Dec (fromIntegral x) Rust.Unsuffixed ()) ()

translateBinaryOp :: Copilot.Expr a -> Copilot.Expr b -> Rust.BinOp -> Rust.Expr ()
translateBinaryOp x y op = Rust.Binary [] op (transExpr x) (transExpr y) ()
