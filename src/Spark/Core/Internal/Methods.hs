{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.Methods where

import Java
import Spark.Core.Internal.Types

-- Start PartialResult

foreign import java unsafe getFinalValue :: (r <: Object) => Java (PartialResult r) r

foreign import java unsafe initialValue :: (r <: Object) => Java (PartialResult r) r

foreign import java unsafe isInitialValueFinal :: (r <: Object) => Java (PartialResult r) r

-- End PartialResult

-- Start BoundedDouble

foreign import java unsafe confidence :: Java BoundedDouble Double

foreign import java unsafe high :: Java BoundedDouble Double

foreign import java unsafe low :: Java BoundedDouble Double

foreign import java unsafe mean :: Java BoundedDouble Double

-- End BoundedDouble

-- Start Optional

foreign import java unsafe get :: (t <: Object) => Java (Optional t) t

foreign import java unsafe isPresent :: (t <: Object) => Java (Optional t) Bool

foreign import java unsafe or :: (t <: Object) => Java (Optional t) t

foreign import java unsafe orElse :: (t <: Object) => Java (Optional t) t

foreign import java unsafe orNull :: (t <: Object) => Java (Optional t) t

-- End Optional

-- Start Partition

foreign import java unsafe "@interface" index :: Java Partition Int

-- End Partition

-- Start StatCounter

foreign import java unsafe copy :: Java StatCounter StatCounter

foreign import java unsafe count :: Java StatCounter Int64

foreign import java unsafe max :: Java StatCounter Double

foreign import java unsafe mean :: Java StatCounter Double

foreign import java unsafe merge :: Double -> Java StatCounter StatCounter

foreign import java unsafe min :: Java StatCounter Double

foreign import java unsafe popStdev :: Java StatCounter Double

foreign import java unsafe popVariance :: Java StatCounter Double

foreign import java unsafe sampleStdev :: Java StatCounter Double

foreign import java unsafe sampleVariance :: Java StatCounter Double

foreign import java unsafe stdev :: Java StatCounter Double

foreign import java unsafe sum :: Java StatCounter Double

foreign import java unsafe variance :: Java StatCounter Double

-- End StatCounter
