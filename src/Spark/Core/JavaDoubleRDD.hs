module Spark.Core.JavaDoubleRDD
  ( module X
  , module Spark.Core.JavaDoubleRDD )
where

import Java
import Spark.Core.Internal.Types
import qualified Spark.Core.Internal.JavaDoubleRDD as S
import qualified Spark.Core.Internal.JavaDoubleRDD as X hiding (filter,
                                                                histogram,
                                                                histogram2,
                                                                histogram3)

filterDouble :: (forall a. JDouble -> Java a JBoolean) -> Java JavaDoubleRDD JavaDoubleRDD
filterDouble t = S.filterDouble (mkFun t)

histogram :: [Double] -> Java JavaDoubleRDD [Int64]
histogram t = fmap fromJava $ S.histogram (toJava t)

histogram2 :: [Double] -> Bool -> Java JavaDoubleRDD [Int64]
histogram2 t1 t2 = fmap fromJava $ S.histogram2 (toJava t1) t2

histogram3 :: Int -> Java JavaDoubleRDD (JDoubleArray, JLongArray)
histogram3 t = fmap fromJava $ S.histogram3 t
