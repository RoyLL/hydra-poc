module Hydra.Architecture.Annotations where

import Data.Data (Data, Typeable)
import Prelude

-- | Defines an annotation type to describe the architectural role of
-- a piece of software.
data Architecture = Component
  deriving (Eq, Show, Read, Data, Typeable)
