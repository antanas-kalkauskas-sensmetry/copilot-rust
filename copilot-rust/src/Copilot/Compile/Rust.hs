-- | Compile Copilot specifications to Rust code.
module Copilot.Compile.Rust
  ( mkDefaultCSettings
  ) where

-- Internal imports
--import Copilot.Compile.Rust.Compile  ( compile, compileWith )
import Copilot.Compile.Rust.Settings ( CSettings (..), mkDefaultCSettings )
