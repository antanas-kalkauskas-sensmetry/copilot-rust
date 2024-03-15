-- | Settings used by the code generator to customize the code.
module Copilot.Compile.Rust.Settings
    ( RustSettings(..)
    , mkDefaultRustSettings
    )
  where

-- | Settings used to customize the code generated.
data RustSettings = RustSettings
  { rustMonitorStructName :: String
  , rustSettingsOutputDirectory  :: FilePath
  }

-- | Default settings with a step function called @step@.
mkDefaultRustSettings :: RustSettings
mkDefaultRustSettings = RustSettings "Monitor" "."
