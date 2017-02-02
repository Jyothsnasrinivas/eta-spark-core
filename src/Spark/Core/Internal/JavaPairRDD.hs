{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.JavaPairRDD where

import Java

data {-# CLASS "org.apache.spark.api.java.JavaPairRDD" #-} JavaPairRDD k v = JavaRDD (Object# (JavaRDD k v)
  deriving Class

foreign import java unsafe "@static org.apache.spark.api.java.JavaPairRDD.aggregate"
  aggregate :: (t <: Object, u <: Object) => u -> Function2 u t u -> Function2 u u u -> Java a u

foreign import java unsafe aggregateByKey :: (k <: Object, v <: Object, u <: Object) => u -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe aggregateByKey2 :: (k <: Object, v <: Object, u <: Object) => u -> Int -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe aggregateByKey3:: (k <: Object, v <: Object, u <: Object) => u -> Partitioner -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe cache :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe coalesce :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe coalesce2 :: (k <: Object, v <: Object) => Int -> Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)
