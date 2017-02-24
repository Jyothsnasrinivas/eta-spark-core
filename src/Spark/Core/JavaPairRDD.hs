{-# LANGUAGE MagicHash #-}
module Spark.Core.JavaPairRDD where

import Java
import qualified Spark.Core.JavaPairRDD as S

foreign import java unsafe "@static org.apache.spark.api.java.JavaPairRDD.aggregate"
aggregate :: (t <: Object, u <: Object) => u -> Function2 u t u -> Function2 u u u -> Java a u

aggregateByKey :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey t1 t2 t3 = S.aggregateByKey t1 (mkFun2 t2) (mkFun2 t3)

aggregateByKey2 :: (k <: Object, v <: Object, u <: Object) => u -> Int -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey2 t1 t2 t3 t4 = S.aggregateByKey t1 t2 (mkFun2 t3) (mkFun2 t4)

aggregateByKey3:: (k <: Object, v <: Object, u <: Object) => u -> Partitioner -> (forall a. u -> v -> Java a u)
                                                               -> (forall a. u -> u -> Java a u)
                                                               -> Java (JavaPairRDD k v) (JavaPairRDD k u)
aggregateByKey3 t1 t2 t3 t4 = S.aggregateByKey3 t1 t2 (mkFun2 t3) (mkFun2 t4)

foreign import java unsafe cache :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe coalesce :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe coalesce2 :: (k <: Object, v <: Object) => Int -> Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe cogroup :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe cogroup2 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe cogroup3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 (Iterable v) (Iterable w)))

foreign import java unsafe cogroup4 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe cogroup5 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

foreign import java unsafe cogroup6 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe cogroup7 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe cogroup8 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple4 (Iterable v) (Iterable w1) (Iterable w2) (Iterable w3)))

foreign import java unsafe cogroup9 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Partitiner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple3 (Iterable v) (Iterable w1) (Iterable w2)))

collectAsMap :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, v)]
collectAsMap = fmap fromJava S.collectAsMap

combineByKey :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                        -> (forall a. c -> v -> Java a c)
                                                        -> (forall a. c -> c -> Java a c)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k v)
combineByKey t1 t2 t3 = S.combineByKey (mkFun t1) (mkFun2 t2) (mkFun2 t3)

combineByKey2 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                          -> (forall a. c -> v -> Java a c)
                                                          -> (forall a. c -> c -> Java a c)
                                                          -> Int
                                                          -> Java (JavaPairRDD k v) (JavaPairRDD k v)
combineByKey2 t1 t2 t3 t4= S.combineByKey2 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey3 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                        -> (forall a. c -> v -> Java a c)
                                                        -> (forall a. c -> c -> Java a c)
                                                        -> Partitioner
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k v)
combineByKey3 t1 t2 t3 t4 = S.combineByKey3 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4

combineByKey4 :: (k <: Object, v <: Object, c <: Object) => (forall a. v -> Java a c)
                                                        -> (forall a. c -> v -> Java a c)
                                                        -> (forall a. c -> c -> Java a c)
                                                        -> Partitioner
                                                        -> Bool
                                                        -> Serializer
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k v)
combineByKey4 t1 t2 t3 t4 t5 t6 = S.combineByKey3 (mkFun t1) (mkFun2 t2) (mkFun2 t3) t4 t5 t6

foreign import java unsafe countApproxDistinctByKey :: (k <: Object, v <: Object) => Double -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

foreign import java unsafe countApproxDistinctByKey2 :: (k <: Object, v <: Object) => Double -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

foreign import java unsafe countApproxDistinctByKey3 :: (k <: Object, v <: Object) => Double -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k JLong)

countByKey :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) [(k, Int64)]
countByKey = fmap (map (fromJava . snd) . fromJava) S.countByKey

foreign import java unsafe countByKeyApprox :: (k <: Object, v <: Object) => Int64 -> Java (JavaPairRDD k v) (PartialResult (Map k BoundedDouble)) --todo

foreign import java unsafe countByKeyApprox2 :: (k <: Object, v <: Object) => Int64 -> Double -> Java (JavaPairRDD k v) (PartialResult (Map k BoundedDouble)) --todo 

foreign import java unsafe distinct :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe distinct2 :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

filter :: (k <: Object, v <: Object) => (forall a. Tuple2 k v -> Java a JBoolean)
                                     -> Java (JavaPairRDD k v) (JavaPairRDD k v
filter t = S.filter (mkFun t)

foreign import java unsafe first :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (Tuple2 k v)

flatMapValues :: (k <: Object, v <: Object, u <: Object) => u -> (forall a. v -> Java a (Iterable u))
                                                        -> (forall a. u -> u -> Java a u)
                                                        -> Java (JavaPairRDD k v) (JavaPairRDD k v)
flatMapValues t1 t2 t3 = S.flatMapValues t1 (mkFun t2) (mkFun2 t3)

foldByKey :: (k <: Object, v <: Object) => v -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey t1 t2 = S.foldByKey t1 (mkFun2 t2)

foldByKey2 :: (k <: Object, v <: Object) => v -> Int -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey2 t1 t2 t3 = S.foldByKey2 t1 t2 (mkFun2 t3)

foreign import java unsafe foldByKey2 :: (k <: Object, v <: Object) => v -> Int -> Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foldByKey3 :: (k <: Object, v <: Object) => v -> Partitioner -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
foldByKey3 t1 t2 t3 = S.foldByKey2 t1 t2 (mkFun2 t3)

foreign import java unsafe fullOuterJoin :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2(Optional v) Optional(w))

foreign import java unsafe fullOuterJoin2 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int-> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2(Optional v) Optional(w))

foreign import java unsafe fullOuterJoin3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2(Optional v) Optional(w))

foreign import java unsafe groupByKey :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe groupByKey2 :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe groupByKey :: (k <: Object, v <: Object) => Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Iterable v))

foreign import java unsafe groupWith :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2((Iterable v)(Iterable w)))

foreign import java unsafe groupWith2 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2((Iterable v)(Iterable w1)(Iterable w2)))

foreign import java unsafe groupWith3 :: (k <: Object, v <: Object, w1 <: Object, w2 <: Object, w3 <: Object) => JavaPairRDD k w1 -> JavaPairRDD k w2 -> JavaPairRDD k w3 -> Java (JavaPairRDD k v) (JavaPairRDD k Tuple2((Iterable v)(Iterable w1)(Iterable w2)(Iterable w3)))

foreign import java unsafe intersection :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe join :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

foreign import java unsafe join2 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

foreign import java unsafe join3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v w))

foreign import java unsafe kClassTag :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (ClassTag k)

foreign import java unsafe keys :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaRDD k)

foreign import java unsafe leftOuterJoin :: (k <: Object, v <: Object, w <: Object) => JavaRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe leftOuterJoin2 :: (k <: Object, v <: Object, w <: Object) => JavaRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe leftOuterJoin3 :: (k <: Object, v <: Object, w <: Object) => JavaRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v (Optional w)))

foreign import java unsafe lookup :: (k <: Object, v <: Object) => k -> Java (JavaPairRDD k v) (List v)

mapValues :: (k <: Object, v <: Object) => u -> (forall a. v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
mapValues t1 t2 = S.mapValues t1 (mkFun t2)

foreign import java unsafe mapValues :: (k <: Object, v <: Object, u <: Object) => u -> Function v u -> Java (JavaPairRDD k v) (JavaPairRDD k u)

foreign import java unsafe name :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe persist :: (k <: Object, v <: Object) => StorageLevel -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe pipe :: (k <: Object, v <: Object) => String -> Java (JavaPairRDD k v) (RDD (Tuple2 k v))

reduceByKey :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey t = S.reduceByKey (mkFun2 t)

foreign import java unsafe reduceByKey :: (k <: Object, v <: Object) => Function2 v v v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

reduceByKey2 :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v) -> Int
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey2 t1 t2 = S.reduceByKey2 (mkFun2 t1) t2

reduceByKey3 :: (k <: Object, v <: Object) => -> Partitioner -> (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKey3 t1 t2 = S.reduceByKey3 t1 (mkFun2 t2)

reduceByKeyLocally :: (k <: Object, v <: Object) => (forall a. v -> v -> Java a v)
                                             -> Java (JavaPairRDD k v) (JavaPairRDD k v)
reduceByKeyLocally t = S.reduceByKeyLocally (mkFun2 t)

foreign import java unsafe repartition :: (k <: Object, v <: Object) => Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe repartitionAndSortWithinPartitions :: (k <: Object, v <: Object) => Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe repartitionAndSortWithinPartitions2 :: (k <: Object, v <: Object) => Partitioner -> Comparator k -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe rightOuterJoin :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v) w)

foreign import java unsafe rightOuterJoin2 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v) w)

foreign import java unsafe rightOuterJoin3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k (Tuple2 v) w)

foreign import java unsafe sample :: (k <: Object, v <: Object) => Bool -> Double -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sample2 :: (k <: Object, v <: Object) => Bool -> Double -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

sampleByKey :: (k <: Object, v <: Object) => Bool -> [(k, JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKey t1 t2 = S.sampleByKey t1 (toJava t2)

foreign import java unsafe sampleByKey :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Java (JavaPairRDD k v) (JavaPairRDD k v)

sampleByKey2 :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKey2 t1 t2 t3 = S.sampleByKey2 t1 (toJava t2) t3

foreign import java unsafe sampleByKey2 :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

sampleByKeyExact :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)]
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExact t1 t2 = S.sampleByKeyExact t1 (toJava t2)

foreign import java unsafe sampleByKeyExact :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Java (JavaPairRDD k v) (JavaPairRDD k v)

sampleByKeyExact2 :: (k <: Object, v <: Object) => Bool -> [(k ,JDouble)] -> Int64
                                                  -> Java (JavaPairRDD k v) (JavaPairRDD k v)
sampleByKeyExact2 t1 t2 t3 = S.sampleByKeyExact2 t1 (toJava t2) t3

foreign import java unsafe sampleByKeyExact2 :: (k <: Object, v <: Object) => Bool -> Map k JDouble -> Int64 -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe saveAsHadoopDataset :: (k <: Object, v <: Object) => JobConf -> Java (JavaPairRDD k v) ()

foreign import java unsafe setName :: (k <: Object, v <: Object) => JString -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey2 :: (k <: Object, v <: Object) => Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey3 :: (k <: Object, v <: Object) => Bool -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey4 :: (k <: Object, v <: Object) => Comparator k -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey5 :: (k <: Object, v <: Object) => Comparator k -> Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe sortByKey6 :: (k <: Object, v <: Object) => Comparator k -> Bool -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtract :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtract2 :: (k <: Object, v <: Object) => JavaPairRDD k v -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtract3 :: (k <: Object, v <: Object) => JavaPairRDD k v -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtractByKey :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtractByKey2 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Int -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe subtractByKey3 :: (k <: Object, v <: Object, w <: Object) => JavaPairRDD k w -> Partitioner -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe union :: (k <: Object, v <: Object) => JavaPairRDD k v -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe unpersist :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe unpersist :: (k <: Object, v <: Object) => Bool -> Java (JavaPairRDD k v) (JavaPairRDD k v)

foreign import java unsafe values :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (JavaRDD v)

foreign import java unsafe vClassTag :: (k <: Object, v <: Object) => Java (JavaPairRDD k v) (ClassTag v)

foreign import java unsafe wrapRDD :: (k <: Object, v <: Object) => RDD (Tuple2 k v) -> Java (JavaPairRDD k v) (JavaPairRDD k v)
