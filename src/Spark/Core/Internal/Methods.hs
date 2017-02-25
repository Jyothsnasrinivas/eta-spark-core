{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.Methods where

import Java
import Spark.Core.Internal.Types

-- PartialResult
foreign import java unsafe getFinalValue :: (r <: Object) => Java (PartialResult r) r

foreign import java unsafe initialValue :: (r <: Object) => Java (PartialResult r) r

foreign import java unsafe isInitialValueFinal :: (r <: Object) => Java (PartialResult r) r

-- End PartialResult
