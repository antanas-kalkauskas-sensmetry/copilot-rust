-- | Compile Copilot specifications to Rust code.
module Copilot.Compile.Rust
  ( mkDefaultRustSettings
  , compile
  ) where

-- Internal imports
import Copilot.Compile.Rust.Compile  ( compile )
import Copilot.Compile.Rust.Settings ( RustSettings (..), mkDefaultRustSettings )
