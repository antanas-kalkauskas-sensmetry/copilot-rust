-- | Compile Copilot specifications to Rust code.
module Copilot.Compile.Rust
  ( mkDefaultRustSettings
  , compile
  , compileWith
  ) where

-- Internal imports
import Copilot.Compile.Rust.Compile  ( compile, compileWith )
import Copilot.Compile.Rust.Settings ( RustSettings (..), mkDefaultRustSettings )
