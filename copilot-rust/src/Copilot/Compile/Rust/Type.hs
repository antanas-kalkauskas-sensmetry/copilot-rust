{-# LANGUAGE GADTs, OverloadedStrings, OverloadedLists #-}

-- | Translate Copilot Core expressions and operators to Rust.
module Copilot.Compile.Rust.Type
    ( transType
    , transLocalVarDeclType
    )
  where

-- External imports
import qualified Language.Rust.Syntax as R

-- Internal imports: Copilot
import Copilot.Core ( Type (..), typeLength, typeName )

-- | Translate a Copilot type to a Rust type.
transType :: Type a -> R.Ty ()
transType ty = case ty of
  Bool     -> mkTypeDef "bool"
  Int8     -> mkTypeDef "i8"
  Int16    -> mkTypeDef "i16"
  Int32    -> mkTypeDef "i32"
  Int64    -> mkTypeDef "i64"
  Word8    -> mkTypeDef "u8"
  Word16   -> mkTypeDef "u16"
  Word32   -> mkTypeDef "u32"
  Word64   -> mkTypeDef "u64"
  Float    -> mkTypeDef "f32"
  Double   -> mkTypeDef "f64"
  Array _  -> error "Arrays are not implemented yet"
  Struct _ -> error "Structs are not implemented yet"
  where
   mkTypeDef name = R.PathTy Nothing (R.Path False [R.PathSegment name Nothing ()] ()) ()

-- | Translate a Copilot type to a valid (local) variable declaration Rust type.
--
-- If the type denotes an array, translate it to a pointer to whatever the
-- array holds. This special case is needed when the type is used for a local
-- variable declaration. We treat global variables differently (we generate
-- list initializers).
transLocalVarDeclType :: Type a -> R.Ty ()
transLocalVarDeclType (Array _) = error "Arrays are not implemented yet"
transLocalVarDeclType ty        = transType ty
