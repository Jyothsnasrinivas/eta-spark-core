{-# LANGUAGE MagicHash #-}

module Spark.Core.Internal.JavaNewHadoopRDD where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe kClassTag :: (k <: Object, v <: Object) => Java (JavaNewHadoopRDD k v) (ClassTag k)

foreign import java unsafe mapPartitionWithInputSplit :: (k <: Object, v <: Object, r <: Object) => Function2 InputSplit (Iterator (Tuple2 k v)) -> Iterator r -> Bool -> Java (JavaNewHadoopRDD k v) (ClassTag k)

foreign import java unsafe vClassTag :: (k <: Object, v <: Object) => Java (JavaNewHadoopRDD k v) (ClassTag v)
