module Spark.Core.Internal.JavaPairRDD where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe "aggregateByKey" aggregateByKeyPair :: (k <: Object, v <: Object, u <: Object) => u -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe "aggregateByKey" aggregateByKey2Pair :: (k <: Object, v <: Object, u <: Object) => u -> Int -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe "aggregateByKey" aggregateByKey3Pair :: (k <: Object, v <: Object, u <: Object) => u -> Partitioner -> Function2 u v u -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe "cache" cachePair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "coalesce" coalescePair :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "coalesce" coalesce2Pair :: (k <: Object, v <: Object) => Int -> Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "cogroup" cogroupPair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe "cogroup" cogroup2Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe "cogroup" cogroup3Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe "cogroup" cogroup4Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe "cogroup" cogroup5Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe "cogroup" cogroup6Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe "cogroup" cogroup7Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe "cogroup" cogroup8Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe "cogroup" cogroup9Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe "collectAsMap" collectAsMapPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (Map k v)

foreign import java unsafe "combineByKey" combineByKeyPair :: (k <: Object, v <: Object, c <: Object) => Function v c -> Function2 c v c -> Function2 c c c -> Java (JavaPairRDD k v) (JavaPairRDD k c)

foreign import java unsafe "combineByKey" combineByKey2Pair :: (k <: Object, v <: Object, c <: Object) => Function v c -> Function2 c v c -> Function2 c c c -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k c)

foreign import java unsafe "combineByKey" combineByKey3Pair :: (k <: Object, v <: Object, c <: Object) => Function v c -> Function2 c v c -> Function2 c c c -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k c)

foreign import java unsafe "combineByKey" combineByKey4Pair :: (k <: Object, v <: Object, c <: Object) => Function v c -> Function2 c v c -> Function2 c c c -> Partitioner -> Bool -> Serializer -> Java (JavaPairRDD k v) (JavaPairRDD k c)

foreign import java unsafe "countApproxDistinctByKey" countApproxDistinctByKeyPair :: (k <: Object, v <: Object) => Double -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

foreign import java unsafe "countApproxDistinctByKey" countApproxDistinctByKey2Pair :: (k <: Object, v <: Object) => Double -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

foreign import java unsafe "countApproxDistinctByKey" countApproxDistinctByKey3Pair :: (k <: Object, v <: Object) => Double -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

foreign import java unsafe "countByKey" countByKeyPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (Map k JLong)

foreign import java unsafe "countByKeyApprox" countByKeyApproxPair :: (k <: Object, v <: Object) => Int64 -> Java (JavaPairRDD k v) (PartialResult (Map k BoundedDouble))

foreign import java unsafe "countByKeyApprox" countByKeyApprox2Pair :: (k <: Object, v <: Object) => Int64 -> Double -> Java (JavaPairRDD k v) (PartialResult (Map k BoundedDouble))

foreign import java unsafe "distinct" distinctPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "distinct" distinct2Pair :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "filter" filterPair :: (k <: Object, v <: Object) => Function (Tuple2 k v) JBoolean -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "first" firstPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (Tuple2 k v)

foreign import java unsafe "flatMapValues" flatMapValuesPair :: (k <: Object, v <: Object, u <: Object) => u -> Function v (Iterable u) -> Function2 u u u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe "foldByKey" foldByKeyPair :: (k <: Object, v <: Object) => v -> Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "foldByKey" foldByKey2Pair :: (k <: Object, v <: Object) => v -> Int -> Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "foldByKey" foldByKey3Pair :: (k <: Object, v <: Object) => v -> Partitioner -> Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "fullOuterJoin" fullOuterJoinPair :: (k <: Object, v <: Object, w <: Object)
                                         => JavaPairRDD k w
                                         -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) (Optional w)))

foreign import java unsafe "fullOuterJoin" fullOuterJoin2Pair :: (k <: Object, v <: Object, w <: Object)
                                          => JavaPairRDD k w -> Int
                                          -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) (Optional w)))

foreign import java unsafe "fullOuterJoin" fullOuterJoin3Pair :: (k <: Object, v <: Object, w <: Object)
                                          => JavaPairRDD k w -> Partitioner
                                          -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) (Optional w)))

foreign import java unsafe "groupByKey" groupByKeyPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe "groupByKey" groupByKey2Pair :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe "groupByKey" groupByKey3Pair :: (k <: Object, v <: Object) => Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe "groupWith" groupWithPair :: (k <: Object, v <: Object, w <: Object)
                                     => JavaPairRDD k w
                                     -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe "groupWith" groupWith2Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object)
                                      => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe "groupWith" groupWith3Pair :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object)
                                      => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3
                                      -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe "intersection" intersectionPair :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "join" joinPair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

foreign import java unsafe "join" join2Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

foreign import java unsafe "join" join3Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

-- foreign import java unsafe kClassTag :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (ClassTag k)

foreign import java unsafe "keys" keysPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaRDD k)

foreign import java unsafe "leftOuterJoin" leftOuterJoinPair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe "leftOuterJoin" leftOuterJoin2Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe "leftOuterJoin" leftOuterJoin3Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe "lookup" lookupPair :: (k <: Object, v <: Object) => k -> Java (JavaPairRDD k v) (List v)

foreign import java unsafe "mapValues" mapValuesPair :: (k <: Object, v <: Object, u <: Object) => Function v u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe "name" namePair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "persist" persistPair :: (k <: Object, v <: Object) => StorageLevel -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "pipe" pipePair :: (k <: Object, v <: Object) => String -> Java (JavaPairRDD k v) (RDD (Tuple2 k v))

foreign import java unsafe "reduceByKey" reduceByKeyPair :: (k <: Object, v <: Object) => Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "reduceByKey" reduceByKey2Pair :: (k <: Object, v <: Object) => Function2 v v v -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "reduceByKey" reduceByKey3Pair :: (k <: Object, v <: Object) => Partitioner -> Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "reduceByKeyLocally" reduceByKeyLocallyPair :: (k <: Object, v <: Object) => Function2 v v v -> Java (JavaPairRDD k v) (Map k v)

foreign import java unsafe "repartition" repartitionPair :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "repartitionAndSortWithinPartitions" repartitionAndSortWithinPartitionsPair :: (k <: Object, v <: Object) => Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "repartitionAndSortWithinPartitions" repartitionAndSortWithinPartitions2Pair :: (k <: Object, v <: Object) => Partitioner -> Comparator k -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "rightOuterJoin" rightOuterJoinPair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) w))

foreign import java unsafe "rightOuterJoin" rightOuterJoin2Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) w))

foreign import java unsafe "rightOuterJoin" rightOuterJoin3Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Optional v) w))

foreign import java unsafe "sample" samplePair :: (k <: Object, v <: Object) => Bool -> Double -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sample" sample2Pair :: (k <: Object, v <: Object) => Bool -> Double -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sampleByKey" sampleByKeyPair :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sampleByKey" sampleByKey2Pair :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sampleByKeyExact" sampleByKeyExactPair :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sampleByKeyExact" sampleByKeyExact2Pair :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "saveAsHadoopDataset" saveAsHadoopDatasetPair :: (k <: Object, v <: Object) => JobConf -> Java (JavaPairRDD k v) ()

foreign import java unsafe "setName" setNamePair :: (k <: Object, v <: Object) => String -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKeyPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKey2Pair :: (k <: Object, v <: Object) => Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKey3Pair :: (k <: Object, v <: Object) => Bool -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKey4Pair :: (k <: Object, v <: Object) => Comparator k -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKey5Pair :: (k <: Object, v <: Object) => Comparator k -> Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "sortByKey" sortByKey6Pair :: (k <: Object, v <: Object) => Comparator k -> Bool -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtract" subtractPair :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtract" subtract2Pair :: (k <: Object, v <: Object) => JavaPairRDD k v -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtract" subtract3Pair :: (k <: Object, v <: Object) => JavaPairRDD k v -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtractByKey" subtractByKeyPair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtractByKey" subtractByKey2Pair :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "subtractByKey" subtractByKey3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "union" unionPair :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "unpersist" unpersistPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "unpersist" unpersist2Pair :: (k <: Object, v <: Object) => Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe "values" valuesPair :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaRDD v)

-- foreign import java unsafe vClassTag :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (ClassTag v)

foreign import java unsafe "wrapRDD" wrapRDDPair :: (k <: Object, v <: Object) => RDD (Tuple2 k v) -> Java (JavaPairRDD k v) (JavaPairRDD k v)
