module Spark.Core.JavaNewHadoopRDD
  ( module X
  , module Spark.Core.JavaNewHadoopRDD )
where

import Java
import Spark.Core.Internal.Types
import qualified Spark.Core.Internal.JavaNewHadoopRDD as S
import qualified Spark.Core.Internal.JavaNewHadoopRDD as X hiding (mapPartitionWithInputSplit)

mapPartitionWithInputSplitNew :: (k <: Object, v <: Object, r <: Object)
                          => (forall a. NewInputSplit -> (Iterator (Tuple2 k v)) -> Java a (Iterator r))
                          -> Bool
                          -> Java (JavaNewHadoopRDD k v) (JavaRDD r)
mapPartitionWithInputSplitNew t1 t2 = S.mapPartitionWithInputSplitNew (mkFun2 t1) t2
