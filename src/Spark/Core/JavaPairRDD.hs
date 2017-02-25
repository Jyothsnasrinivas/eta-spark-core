{-# LANGUAGE MagicHash #-}
module Spark.Core.JavaPairRDD
  ( module X
  , module Spark.Core.JavaPairRDD )
where

import Java
import Spark.Core.Internal.Types
import Spark.Core.Internal.JavaPairRDD as X hiding
  (aggregateByKey
  ,aggregateByKey2
  ,aggregateByKey3
  ,collectAsMap
  ,combineByKey
  ,combineByKey2
  ,combineByKey3
  ,combineByKey4
  ,countByKey
  ,filter
  ,flatMapValues
  ,foldByKey
  ,foldByKey2
  ,foldByKey3
  ,mapValues
  ,reduceByKey
  ,reduceByKey2
  ,reduceByKey3
  ,reduceByKeyLocally
  ,sampleByKey
  ,sampleByKey2
  ,sampleByKeyExact
  ,sampleByKeyExact2)
import qualified Spark.Core.Internal.JavaPairRDD as S

aggregateByKey :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey t1 t2 t3 = S.aggregateByKey t1 (mkFun2 t2) (mkFun2 t3)

aggregateByKey2 :: (k <: Object, v <: Object, u <: Object) => u -> Int -> (forall a. u -> v -> Java a u)
                                                           -> (forall a. u -> u -> Java a u)
                                                           -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey2 t1 t2 t3 t4 = S.aggregateByKey2 t1 t2 (mkFun2 t3) (mkFun2 t4)

aggregateByKey3:: (k <: Object, v <: Object, u <: Object) => u -> Partitioner -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey3 t1 t2 t3 t4 = S.aggregateByKey3 t1 t2 (mkFun2 t3) (mkFun2 t4)

collectAsMap :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, v)]
collectAsMap = fmap fromJava S.collectAsMap

combineByKey :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                        -> (forall a. c -> v -> Java a c)
                                                        -> (forall a. c -> c -> Java a c)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey t1 t2 t3 = S.combineByKey (mkFun t1) (mkFun2 t2) (mkFun2 t3)

combineByKey2 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                          -> (forall a. c -> v -> Java a c)
                                                          -> (forall a. c -> c -> Java a c)
                                                          -> Int
                                                          -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey2 t1 t2 t3 t4 = S.combineByKey2 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey3 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                         -> (forall a. c -> v -> Java a c)
                                                         -> (forall a. c -> c -> Java a c)
                                                         -> Partitioner
                                                         -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey3 t1 t2 t3 t4 = S.combineByKey3 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey4 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                         -> (forall a. c -> v -> Java a c)
                                                         -> (forall a. c -> c -> Java a c)
                                                         -> Partitioner
                                                         -> Bool
                                                         -> Serializer
                                                         -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey4 t1 t2 t3 t4 t5 t6 = S.combineByKey4 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4 t5 t6

countByKey :: forall k v. (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, Int64)]
countByKey = fmap toList S.countByKey
  where toList = map (\(k, l) -> (k, fromJava l)) . (fromJava :: Map k JLong -> [(k, JLong)])

filter :: (k <: Object, v <: Object) => (forall a. Tuple2 k v -> Java a JBoolean)
                                     -> Java (JavaPairRDD k v) (JavaPairRDD k v)
filter t = S.filter (mkFun t)

flatMapValues :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. v -> Java a (Iterable u))
                                                        -> (forall a. u -> u -> Java a u)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k u)
flatMapValues t1 t2 t3 = S.flatMapValues t1 (mkFun t2) (mkFun2 t3)

foldByKey :: (k <: Object, v <: Object) => v -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey t1 t2 = S.foldByKey t1 (mkFun2 t2)

foldByKey2 :: (k <: Object, v <: Object) => v -> Int -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey2 t1 t2 t3 = S.foldByKey2 t1 t2 (mkFun2 t3)

foldByKey3 :: (k <: Object, v <: Object) => v -> Partitioner -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey3 t1 t2 t3 = S.foldByKey3 t1 t2 (mkFun2 t3)

mapValues :: (k <: Object, v <: Object, u <: Object) => (forall a. v -> Java a u)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k u)
mapValues t1 = S.mapValues (mkFun t1)

reduceByKey :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey t = S.reduceByKey (mkFun2 t)

reduceByKey2 :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v) -> Int
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey2 t1 t2 = S.reduceByKey2 (mkFun2 t1) t2

reduceByKey3 :: (k <: Object, v <: Object) => Partitioner -> (forall a. v -> v -> Java a v)
                                           -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey3 t1 t2 = S.reduceByKey3 t1 (mkFun2 t2)

reduceByKeyLocally :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (Map k v)
reduceByKeyLocally t = S.reduceByKeyLocally (mkFun2 t)

sampleByKey :: (k <: Object, v <: Object) => Bool -> [(k, JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKey t1 t2 = S.sampleByKey t1 (toJava t2)

sampleByKey2 :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKey2 t1 t2 t3 = S.sampleByKey2 t1 (toJava t2) t3

sampleByKeyExact :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExact t1 t2 = S.sampleByKeyExact t1 (toJava t2)

sampleByKeyExact2 :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExact2 t1 t2 t3 = S.sampleByKeyExact2 t1 (toJava t2) t3
