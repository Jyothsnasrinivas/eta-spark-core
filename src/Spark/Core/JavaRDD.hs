module Spark.Core.JavaRDD
  ( module X
  , module Spark.Core.JavaRDD )
where

import Java
import Spark.Core.Internal.Types
import Spark.Core.Internal.JavaRDD as X hiding (filter, randomSplit, randomSplit2, sortBy)
import qualified Spark.Core.Internal.JavaRDD as S

filter :: (t <: Object) => (forall a. t -> Java a JBoolean) -> Java (JavaRDD t) (JavaRDD t)
filter f = S.filter (mkFun f)

randomSplit :: (t <: Object) => [Double] -> Java (JavaRDD t) [JavaRDD t]
randomSplit t = fmap fromJava (S.randomSplit (toJava t))

randomSplit2 :: (t <: Object) => [Double] -> Int64 -> Java (JavaRDD t) [JavaRDD t]
randomSplit2 t1 t2 = fmap fromJava (S.randomSplit2 (toJava t1) t2)

sortBy :: (t <: Object, s <: Object) => (forall a. t -> Java a s) -> Bool -> Int -> Java (JavaRDD t) (JavaRDD t)
sortBy t1 t2 t3 = S.sortBy (mkFun t1) t2 t3
