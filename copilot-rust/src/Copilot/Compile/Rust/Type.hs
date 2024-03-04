{-# LANGUAGE GADTs, OverloadedStrings, OverloadedLists #-}

-- | Translate Copilot Core expressions and operators to Rust.
module Copilot.Compile.Rust.Type
    ( transType
    , transTypeR
    , transLocalVarDeclType
    , transLocalVarDeclTypeR
    , transTypeName
    )
  where

-- External imports
import qualified Language.C99.Simple as C
import qualified Language.Rust.Syntax as R

-- Internal imports: Copilot
import Copilot.Core ( Type (..), typeLength, typeName )

-- | Translate a Copilot type to a Rust type.
transType :: Type a -> C.Type
transType ty = case ty of
  Bool      -> C.TypeSpec $ C.TypedefName "bool"
  Int8      -> C.TypeSpec $ C.TypedefName "int8_t"
  Int16     -> C.TypeSpec $ C.TypedefName "int16_t"
  Int32     -> C.TypeSpec $ C.TypedefName "int32_t"
  Int64     -> C.TypeSpec $ C.TypedefName "int64_t"
  Word8     -> C.TypeSpec $ C.TypedefName "uint8_t"
  Word16    -> C.TypeSpec $ C.TypedefName "uint16_t"
  Word32    -> C.TypeSpec $ C.TypedefName "uint32_t"
  Word64    -> C.TypeSpec $ C.TypedefName "uint64_t"
  Float     -> C.TypeSpec C.Float
  Double    -> C.TypeSpec C.Double
  Array ty' -> C.Array (transType ty') len
    where
      len = Just $ C.LitInt $ fromIntegral $ typeLength ty
  Struct s  -> C.TypeSpec $ C.Struct (typeName s)

-- | Translate a Copilot type to a Rust type.
transTypeR :: Type a -> R.Ty ()
transTypeR ty = case ty of
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
transLocalVarDeclType :: Type a -> C.Type
transLocalVarDeclType (Array ty') = C.Ptr $ transType ty'
transLocalVarDeclType ty          = transType ty

transLocalVarDeclTypeR :: Type a -> R.Ty ()
transLocalVarDeclTypeR (Array _) = error "Arrays are not implemented yet"
transLocalVarDeclTypeR ty        = transTypeR ty

-- | Translate a Copilot type intro a C typename
transTypeName :: Type a -> C.TypeName
transTypeName ty = C.TypeName $ transType ty
