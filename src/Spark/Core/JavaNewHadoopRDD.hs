{-# LANGUAGE MagicHash #-}

module Spark.Core.JavaNewHadoopRDD
  ( module X
  , module Spark.Core.JavaNewHadoopRDD )
where

import Java
import Spark.Core.Internal.Types
import qualified Spark.Core.Internal.JavaNewHadoopRDD as S
import qualified Spark.Core.Internal.JavaNewHadoopRDD as X hiding (mapPartitionWithInputSplit)

mapPartitionWithInputSplit :: (k <: Object, v <: Object, r <: Object)
                          => (forall a. NewInputSplit -> (Iterator (Tuple2 k v)) -> Java a (Iterator r))
                          -> Bool
                          -> Java (JavaNewHadoopRDD k v) (JavaRDD r)
mapPartitionWithInputSplit t1 t2 = S.mapPartitionWithInputSplit (mkFun2 t1) t2
