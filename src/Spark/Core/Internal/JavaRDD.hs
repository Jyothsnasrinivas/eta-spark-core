{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.JavaRDD where

import Java

data {-# CLASS "org.apache.spark.api.java.JavaRDD" #-} JavaRDD t = JavaRDD (Object# (JavaRDD t))
  deriving Class

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.aggregate"
  aggregate :: (t <: Object, u <: Object) => u -> Function2 u t u -> Function2 u u u -> Java a u

foreign import java unsafe cache :: (t <: Object) => Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.cartesian"
  cartesian :: (t <: Object, u <: Object) => JavaRDDLike u b -> Java a (JavaPairRDD t u)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.checkpoint"
  checkpoint :: (t <: Object) => Java a ()

foreign import java unsafe classTag :: (t <: Object) => Java (JavaRDD t) (ClassTag t)

foreign import java unsafe coalesce :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe coalesce2 :: (t <: Object) => Int -> Bool -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.collect"
  collect :: (t <: Object) => Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.collectAsync"
  collectAsync :: (t <: Object) => Java a (JavaFutureAction (List t))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.collectPartitions"
  collectPartitions :: (t <: Object) => JIntArray -> Java a (List t) -- TODO: Array Note ********

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.context"
  context :: (t <: Object) => Java a SparkContext

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.count"
  count :: (t <: Object) => Java a Int64

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countApprox"
  countApprox :: (t <: Object) => Int64 -> Java a (PartialResult BoundedDouble)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countApprox2"
  countApprox2 :: (t <: Object) => Int64 -> Double -> Java a (PartialResult BoundedDouble)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countApproxDistinct"
  countApproxDistinct :: (t <: Object) => Double -> Java a Int64

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countAsync"
  countAsync :: (t <: Object) => Java a (JavaFutureAction JLong)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countByValue"
  countByValue :: (t <: Object) => Java a (Map t JLong)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countByValueApprox"
  countbyValueApprox :: (t <: Object) => Int64 -> Java a (PartialResult Map(t BoundedDouble))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.countbyValueApprox2"
  countbyValueApprox2 :: (t <: Object) => Int64 -> Double -> Java a (PartialResult Map(t BoundedDouble)

foreign import java unsafe distinct :: (t <: Object) => Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe distinct2 :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe filter :: (t <: Object) => Function t JBoolean -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.first"
  first :: (t <: Object) => Java a t

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.flatMap"
  flatMap :: (t <: Object, u <: Object) => FlatMapFunction t u -> Java a (JavaRDD u)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.flatMapToDouble"
  flatMapToDouble :: (t <: Object) => DoubleFlatMapFunction t -> Java a JavaDoubleRDD

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.flatMapToPair"
  flatMapToPair :: (t <: Object, k2 <: Object, v2 <: Object)
                => PairFlatMapFunction t k2 v2 -> Java a (JavaPairRDD k2 v2)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.fold"
  fold :: (t <: Object) => -> t -> Function2 t t t -> Java a t

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.foreach"
  foreach :: (t <: Object) => VoidFunction t -> Java a ()

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.foreachAsync"
  foreachAsync :: (t <: Object) => VoidFunction t -> Java a (JavaFutureAction Void)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.foreachPartition"
  foreachPartition :: (t <: Object) => VoidFunction (Iterator t) -> Java a ()

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.foreachAsyncPartition"
  foreachAsyncPartition :: (t <: Object) => VoidFunction (Iterator t) -> Java a (JavaFutureAction Void)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.fromRDD"
  fromRDD :: (t <: Object) => RDD t -> ClassTag t -> Java a (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.getCheckpointFile"
  getCheckpointFile :: (t <: Object) => Java a (Optional JString)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.getNumPartitions"
  getNumPartitions :: (t <: Object) => Java a Int

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.getStorageLevel"
  getStorageLevel :: (t <: Object) => Java a StorageLevel

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.glom"
  glom :: (t <: Object) => Java a (JavaRDD (List t))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.groupBy"
  groupBy :: (t <: Object, u <: Object) => Function t u -> Java a (JavaPairRDD u Iterable(t))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.groupBy2"
  groupBy2 :: (t <: Object, u <: Object) => Function t u -> Int -> Java a (JavaPairRDD u Iterable(t))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.id"
  id :: (t <: Object) => Java a Int

foreign import java unsafe intersection :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.isCheckpointed"
  isCheckpointed :: (t <: Object) => Java a Bool

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.isEmpty"
  isEmpty :: (t <: Object) => Java a Bool

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.iterator"
  iterator :: (t <: Object) => Partition -> TaskContext -> Java a (Iterator t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.keyBy"
  keyBy :: (t <: Object, u <: Object) => Function t u -> Java a (JavaPairRDD u t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.map"
  map :: (t <: Object, r <: Object) => Function t r -> Java a (JavaRDD r)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitions"
  mapPartitions :: (t <: Object, u <: Object) => FlatMapFunction (Iterator t) u -> Java a (JavaRDD u)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitions2"
  mapPartitions2 :: (t <: Object, u <: Object) => FlatMapFunction (Iterator t) u -> Bool -> Java a (JavaRDD u)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitionsToDouble"
  mapPartitionsToDouble :: (t <: Object) => DoubleFlatMapFunction (Iterator t) -> Java a JavaDoubleRDD

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitionsToDouble2"
  mapPartitionsToDouble2 :: (t <: Object) => DoubleFlatMapFunction (Iterator t) -> Bool -> Java a JavaDoubleRDD

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitionsToPair"
  mapPartitionsToPair :: (t <: Object, k2 <: Object, v2 <: Object)
                      => PairFlatMapFunction (Iterator t) k2 v2 -> Java a (JavaPairRDD k2 v2)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitionsToPair2"
  mapPartitionsToPair2 :: (t <: Object, k2 <: Object, v2 <: Object)
                       => PairFlatMapFunction (Iterator t) k2 v2 -> Bool -> Java a (JavaPairRDD k2 v2)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapPartitionsWithIndex"
  map :: (t <: Object, r <: Object) => Function2 JInteger (Iterator t) (Iterator r) -> Bool -> Java a (JavaRDD r)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapToDouble"
  mapToDouble :: (t <: Object) => DoubleFunction t -> Java a JavaDoubleRDD

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.mapToPair"
  mapToPair :: (t <: Object, k2 <: Object, v2 <: Object)
                => PairFunction t k2 v2 -> Java a (JavaPairRDD k2 v2)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.max"
  max :: (t <: Object) => Comparator t -> Java a t

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.min"
  min :: (t <: Object) => Comparator t -> Java a t

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.name"
  name :: (t <: Object) => Java a JString

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.partitioner"
  partitioner :: (t <: Object) => Java a (Optional Partitioner)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.partitions"
  partitions :: (t <: Object) => Java a (List Partition)

foreign import java unsafe persist :: (t <: Object) => StorageLevel newLevel -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.pipe"
  pipe :: (t <: Object) => List JString -> Java a (JavaRDD JString)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.pipe2"
  pipe2 :: (t <: Object) => List JString -> Map Jstring Jstring -> Java a (JavaRDD JString)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.pipe3"
  pipe3 :: (t <: Object) => List JString -> Map JString JString -> Bool -> Int -> Java a (JavaRDD JString)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.pipe4"
  pipe4 :: (t <: Object) => List JString -> Map JString JString -> Bool -> Int -> JString -> Java a (JavaRDD JString)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.pipe5"
  pipe5 :: (t <: Object) => JString -> Java a (JavaRDD JString)

foreign import java unsafe randomSplit :: (t <: Object) => Double -> Java (JavaRDD t) (JavaRDD t) --TODO: Array Note ********

foreign import java unsafe randomSplit2 :: (t <: Object) => Double -> Int64 -> Java (JavaRDD t) (JavaRDD t) --TODO: Array Note ********

foreign import java unsafe rdd :: (t <: Object) => Java (JavaRDD t) (RDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.reduce"
  reduce :: (t <: Object) => Fuction2 t t t -> Java a t

foreign import java unsafe repartition :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sample :: (t <: Object) => Bool -> Double -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sample2 :: (t <: Object) => Bool -> Double -> Int64 -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.saveAsObjectFile"
  saveAsObjectFile :: (t <: Object) => JString -> Java a ()

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.saveAsTextFile"
  saveAsTextFile :: (t <: Object) => JString -> Java a ()

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.saveAsTextFile2"
  saveAsObjectFile2 :: (t <: Object, b <: CompressionCodec) => JString -> JClass b -> Java a ()

foreign import java unsafe setName :: (t <: Object) => JString -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sortBy :: (t <: Object, s <: Object) => Function t s -> Bool -> Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract2 :: (t <: Object) => JavaRDD t -> Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract3 :: (t <: Object) => JavaRDD t -> Partitioner -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.take"
  take :: (t <: Object) => Int -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.takeAsync"
  takeAsunc :: (t <: Object) => Int -> Java a (JavaFutureAction (List t))

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.takeOrdered"
  collect :: (t <: Object) => Int -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.takeOrdered2"
  takeOrdered2 :: (t <: Object) => Int -> Comparator t -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.takeSample"
  takeSample :: (t <: Object) => Bool -> Int -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.takeSample2"
  takeSample2 :: (t <: Object) => Bool -> Int -> Int64 -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.toDebugString"
  toDebugString :: (t <: Object) => Java a JString

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.toLocalIterator"
  toLocalIterator :: (t <: Object) => Java a (Iterator t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.top"
  top :: (t <: Object) => Int -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.top2"
  top2 :: (t <: Object) => Int -> Comparator t -> Java a (List t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.toRDD"
  toRDD :: (t <: Object) => JavaRDD t -> Java a (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.treeAggregate"
  treeAggregate :: (t <: Object, u <: Object) => u -> Function2 u t u -> Function2 u u u -> Java a u

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.treeAggregate2"
  treeAggregate2 :: (t <: Object, u <: Object) => u -> Function2 u t u -> Function2 u u u -> Int -> Java a u

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.treeReduce"
  treeReduce :: (t <: Object) => Fuction2 t t t -> Java a t

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.treeReduce2"
  treeReduce2 :: (t <: Object) => Fuction2 t t t -> Int -> Java a t

foreign import java unsafe union :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe unpersist :: (t <: Object) =>  Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe unpersist2 :: (t <: Object) => Bool -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe wrapRDD :: (t <: Object) => RDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.zip"
  zip :: (t <: Object, u <: Object) => JavaRDDLike u b -> Java a (JavaPairRDD t u)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.zipPartitions"
    zipPartitions :: (t <: Object, u <: Object, v <: Object)
                  => JavaRDDLike u b -> FlatMapFunction2 (Iterator t) (Iterator u) v -> Java a (JavaRDD v)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.zipWithIndex"
  zipWithIndex :: (t <: Object) => Java a (JavaPairRDD t JLong)

foreign import java unsafe "@static org.apache.spark.api.java.JavaRDD.zipWithUniqueId"
  zipWithUniqueId :: (t <: Object) => Java a (JavaPairRDD t JLong)
