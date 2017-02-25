{-# LANGUAGE MagicHash #-}

module Spark.Core.JavaHadoopRDD
  ( module X
  , module Spark.Core.JavaHadoopRDD )
where

import Java
import Spark.Core.Internal.Types
import qualified Spark.Core.Internal.JavaHadoopRDD as S
import qualified Spark.Core.Internal.JavaHadoopRDD as X hiding (mapPartitionWithInputSplit)

mapPartitionWithInputSplit :: (k <: Object, v <: Object, r <: Object)
                          => (forall a. InputSplit -> (Iterator (Tuple2 k v)) -> Java a (Iterator r))
                          -> Bool
                          -> Java (JavaHadoopRDD k v) (JavaRDD r)
mapPartitionWithInputSplit t1 t2 = S.mapPartitionWithInputSplit (mkFun2 t1) t2
