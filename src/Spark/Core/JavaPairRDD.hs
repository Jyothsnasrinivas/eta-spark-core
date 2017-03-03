
module Spark.Core.JavaPairRDD
  ( module X
  , module Spark.Core.JavaPairRDD )
where

import Java
import Spark.Core.Internal.Types
import Spark.Core.Internal.JavaPairRDD as X hiding
  (aggregateByKeyPair
  ,aggregateByKey2Pair
  ,aggregateByKey3Pair
  ,collectAsMapPair
  ,combineByKeyPair
  ,combineByKey2Pair
  ,combineByKey3Pair
  ,combineByKey4Pair
  ,countByKeyPair
  ,filterPair
  ,flatMapValuesPair
  ,foldByKeyPair
  ,foldByKey2Pair
  ,foldByKey3Pair
  ,mapValuesPair
  ,reduceByKeyPair
  ,reduceByKey2Pair
  ,reduceByKey3Pair
  ,reduceByKeyLocallyPair
  ,sampleByKeyPair
  ,sampleByKey2Pair
  ,sampleByKeyExactPair
  ,sampleByKeyExact2Pair)
import qualified Spark.Core.Internal.JavaPairRDD as S

aggregateByKeyPair :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKeyPair t1 t2 t3 = S.aggregateByKeyPair t1 (mkFun2 t2) (mkFun2 t3)

aggregateByKey2Pair :: (k <: Object, v <: Object, u <: Object) => u -> Int -> (forall a. u -> v -> Java a u)
                                                           -> (forall a. u -> u -> Java a u)
                                                           -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey2Pair t1 t2 t3 t4 = S.aggregateByKey2Pair t1 t2 (mkFun2 t3) (mkFun2 t4)

aggregateByKey3Pair :: (k <: Object, v <: Object, u <: Object) => u -> Partitioner -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey3Pair t1 t2 t3 t4 = S.aggregateByKey3Pair t1 t2 (mkFun2 t3) (mkFun2 t4)

collectAsMapPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, v)]
collectAsMapPair = fmap fromJava S.collectAsMapPair

combineByKeyPair :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                        -> (forall a. c -> v -> Java a c)
                                                        -> (forall a. c -> c -> Java a c)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKeyPair t1 t2 t3 = S.combineByKeyPair (mkFun t1) (mkFun2 t2) (mkFun2 t3)

combineByKey2Pair :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                          -> (forall a. c -> v -> Java a c)
                                                          -> (forall a. c -> c -> Java a c)
                                                          -> Int
                                                          -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey2Pair t1 t2 t3 t4 = S.combineByKey2Pair (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey3Pair :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                         -> (forall a. c -> v -> Java a c)
                                                         -> (forall a. c -> c -> Java a c)
                                                         -> Partitioner
                                                         -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey3Pair t1 t2 t3 t4 = S.combineByKey3Pair (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey4Pair :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                         -> (forall a. c -> v -> Java a c)
                                                         -> (forall a. c -> c -> Java a c)
                                                         -> Partitioner
                                                         -> Bool
                                                         -> Serializer
                                                         -> Java (JavaPairRDD k v) (JavaPairRDD k c)
combineByKey4Pair t1 t2 t3 t4 t5 t6 = S.combineByKey4Pair (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4 t5 t6

countByKeyPair :: forall k v. (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, Int64)]
countByKeyPair = fmap toList S.countByKeyPair
  where toList = map (\(k, l) -> (k, fromJava l)) . (fromJava :: Map k JLong -> [(k, JLong)])

filterPair :: (k <: Object, v <: Object) => (forall a. Tuple2 k v -> Java a JBoolean)
                                     -> Java (JavaPairRDD k v) (JavaPairRDD k v)
filterPair t = S.filterPair (mkFun t)

flatMapValuesPair :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. v -> Java a (Iterable u))
                                                        -> (forall a. u -> u -> Java a u)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k u)
flatMapValuesPair t1 t2 t3 = S.flatMapValuesPair t1 (mkFun t2) (mkFun2 t3)

foldByKeyPair :: (k <: Object, v <: Object) => v -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKeyPair t1 t2 = S.foldByKeyPair t1 (mkFun2 t2)

foldByKey2Pair :: (k <: Object, v <: Object) => v -> Int -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey2Pair t1 t2 t3 = S.foldByKey2Pair t1 t2 (mkFun2 t3)

foldByKey3Pair :: (k <: Object, v <: Object) => v -> Partitioner -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey3Pair t1 t2 t3 = S.foldByKey3Pair t1 t2 (mkFun2 t3)

mapValuesPair :: (k <: Object, v <: Object, u <: Object) => (forall a. v -> Java a u)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k u)
mapValuesPair t1 = S.mapValuesPair (mkFun t1)

reduceByKeyPair :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKeyPair t = S.reduceByKeyPair (mkFun2 t)

reduceByKey2Pair :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v) -> Int
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey2Pair t1 t2 = S.reduceByKey2Pair (mkFun2 t1) t2

reduceByKey3Pair :: (k <: Object, v <: Object) => Partitioner -> (forall a. v -> v -> Java a v)
                                           -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey3Pair t1 t2 = S.reduceByKey3Pair t1 (mkFun2 t2)

reduceByKeyLocallyPair :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (Map k v)
reduceByKeyLocallyPair t = S.reduceByKeyLocallyPair (mkFun2 t)

sampleByKeyPair :: (k <: Object, v <: Object) => Bool -> [(k, JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyPair t1 t2 = S.sampleByKeyPair t1 (toJava t2)

sampleByKey2Pair :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKey2Pair t1 t2 t3 = S.sampleByKey2Pair t1 (toJava t2) t3

sampleByKeyExactPair :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExactPair t1 t2 = S.sampleByKeyExactPair t1 (toJava t2)

sampleByKeyExact2Pair :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExact2Pair t1 t2 t3 = S.sampleByKeyExact2Pair t1 (toJava t2) t3
