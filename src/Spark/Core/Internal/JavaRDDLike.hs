{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.JavaRDDLike where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe aggregateByKey :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => u
                                          -> Function2 u t u -> Function2 u u u -> Java this (u)

foreign import java unsafe cartesian :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => JavaRDDLike u b
                                          -> Java this (JavaRDDLike t u)

foreign import java unsafe checkpoint :: (t <: Object, this <: JavaRDDLike t this) => Java this ()

foreign import java unsafe classTag :: (t <: Object, this <: JavaRDDLike t this) => Java this (ClassTag t)

foreign import java unsafe collect :: (t <: Object, this <: JavaRDDLike t this) => Java this (List t)

foreign import java unsafe collectAsync :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaFutureAction List t)

foreign import java unsafe collectPartitions :: (t <: Object, this <: JavaRDDLike t this) => JIntArray -> Java this (ListArray t)

foreign import java unsafe context :: (t <: Object, this <: JavaRDDLike t this) => Java this (SparkContext)

foreign import java unsafe count :: (t <: Object, this <: JavaRDDLike t this) => Java this (Int64)

foreign import java unsafe countApprox :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Java this (PartialResult BoundedDouble)

foreign import java unsafe countApprox2 :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Double -> Java this (PartialResult BoundedDouble)

foreign import java unsafe countApproxDistinct :: (t <: Object, this <: JavaRDDLike t this) => Double -> Java this (Int64)

foreign import java unsafe countAsync :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaFutureAction Int64)

foreign import java unsafe countByValue :: (t <: Object, this <: JavaRDDLike t this) => Java this (Map t Int64)

foreign import java unsafe countByValueApprox :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Java this (PartialResult (Map t BoundedDouble))

foreign import java unsafe countByValueApprox2 :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Double -> Java this (PartialResult (Map t BoundedDouble))

foreign import java unsafe first:: (t <: Object, this <: JavaRDDLike t this) => Java this t

foreign import java unsafe flatMap :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => FlatMapFunction t u -> Java this (JavaRDD u)

foreign import java unsafe flatMapToDouble :: (t <: Object, this <: JavaRDDLike t this) => DoubleFlatMapFunction -> Java this JavaDoubleRDD

foreign import java unsafe flatMapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object) => PairFlatMapFunction t k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe fold :: (t <: Object, this <: JavaRDDLike t this) => t -> Function2 t t t -> Java this t

foreign import java unsafe foreach :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction t -> Java this ()

foreign import java unsafe foreachAsync :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction t -> Java this (JavaFutureAction ())

foreign import java unsafe foreachPartition :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction (Iterator t) -> Java this ()

foreign import java unsafe foreachPartitionAsync :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction (Iterator t) -> Java this (JavaFutureAction ())

foreign import java unsafe getCheckpointFile :: (t <: Object, this <: JavaRDDLike t this) => Java this (Optional JString)

foreign import java unsafe getNumParitions :: (t <: Object, this <: JavaRDDLike t this) => Java this Int

foreign import java unsafe getStorageLevel :: (t <: Object, this <: JavaRDDLike t this) => Java this StorageLevel

foreign import java unsafe glom :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaRDD (List t))

foreign import java unsafe groupBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Java this (JavaPairRDD u (Iterable t))

foreign import java unsafe groupBy2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Int -> Java this (JavaPairRDD u (Iterable t))

foreign import java unsafe id :: (t <: Object, this <: JavaRDDLike t this) => Java this Int

foreign import java unsafe isCheckpointed :: (t <: Object, this <: JavaRDDLike t this) => Java this Bool

foreign import java unsafe isEmpty :: (t <: Object, this <: JavaRDDLike t this) => Java this Bool

foreign import java unsafe iterator :: (t <: Object, this <: JavaRDDLike t this) => Partition -> TaskContext -> Java this (Iterator t)

foreign import java unsafe keyBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Java this (JavaPairRDD u t)

foreign import java unsafe map :: (t <: Object, this <: JavaRDDLike t this, r <: Object) => Function t r -> Java this (JavaRDD r)

foreign import java unsafe mapPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => FlatMapFunction (Iterator t) u -> Java this (JavaRDD u)

foreign import java unsafe mapPartitions2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => FlatMapFunction (Iterator t) u -> Bool -> Java this (JavaRDD u)

foreign import java unsafe mapPartitionsToDouble :: (t <: Object, this <: JavaRDDLike t this) => DoubleFlatMapFunction (Iterator t) -> Java this JavaDoubleRDD

foreign import java unsafe mapPartitionsToDouble2 :: (t <: Object, this <: JavaRDDLike t this) => DoubleFlatMapFunction (Iterator t) -> Bool -> Java this JavaDoubleRDD

foreign import java unsafe mapPartitionsToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                              => PairFlatMapFunction (Iterator t) k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe mapPartitionsToPair2 :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                               => PairFlatMapFunction (Iterator t) k2 v2 -> Bool -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe mapPartitionsWithIndex :: (t <: Object, this <: JavaRDDLike t this, r <: Object)
                                                  => Function2 JInteger (Iterator t) (Iterator r) -> Bool -> Java this (JavaRDD r)

foreign import java unsafe mapToDouble :: (t <: Object, this <: JavaRDDLike t this, r <: Object) => DoubleFunction -> Java this JavaDoubleRDD

foreign import java unsafe mapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                     => PairFunction t k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe max :: (t <: Object, this <: JavaRDDLike t this) => Comparator t -> Java this t

foreign import java unsafe min :: (t <: Object, this <: JavaRDDLike t this) => Comparator t -> Java this t

foreign import java unsafe name :: (t <: Object, this <: JavaRDDLike t this) => Java this JString

foreign import java unsafe partitioner :: (t <: Object, this <: JavaRDDLike t this) => Java this (Optional Partitioner)

foreign import java unsafe partitions :: (t <: Object, this <: JavaRDDLike t this) => Java this (List Partition)

foreign import java unsafe pipe :: (t <: Object, this <: JavaRDDLike t this) => List JString -> Java this (JavaRDD JString)

foreign import java unsafe pipe2 :: (t <: Object, this <: JavaRDDLike t this) => List JString -> Map JString JString -> Java this (JavaRDD JString)

foreign import java unsafe pipe3 :: (t <: Object, this <: JavaRDDLike t this)
                                 => List JString -> Map JString JString -> Bool -> Int -> Java this (JavaRDD JString)

foreign import java unsafe pipe4 :: (t <: Object, this <: JavaRDDLike t this)
                                => List JString -> Map JString JString -> Bool -> Int -> JString -> Java this (JavaRDD JString)

foreign import java unsafe pipe5 :: (t <: Object, this <: JavaRDDLike t this) => JString -> Java this (JavaRDD JString)

foreign import java unsafe rdd :: (t <: Object, this <: JavaRDDLike t this) => Java this (RDD t)

foreign import java unsafe reduce :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Java this t

foreign import java unsafe saveAsObjectFile :: (t <: Object, this <: JavaRDDLike t this) => JString -> Java this ()

foreign import java unsafe saveAsTextFile :: (t <: Object, this <: JavaRDDLike t this) => JString -> Java this ()

foreign import java unsafe saveAsTextFile :: (t <: Object, this <: JavaRDDLike t this, b <: CompressionCodec) => JString -> Class b -> Java this ()

foreign import java unsafe take :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe takeAsync :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (JavaFutureAction (List t))

foreign import java unsafe takeOrdered :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe takeOrdered2 :: (t <: Object, this <: JavaRDDLike t this) => Int -> Comparator t -> Java this (List t)

foreign import java unsafe takeSample :: (t <: Object, this <: JavaRDDLike t this) => Bool -> Int -> Java this (List t)

foreign import java unsafe takeSample2 :: (t <: Object, this <: JavaRDDLike t this) => Bool -> Int -> Int64 -> Java this (List t)

foreign import java unsafe toDebugString :: (t <: Object, this <: JavaRDDLike t this) => Java this JString

foreign import java unsafe toLocalIterator :: (t <: Object, this <: JavaRDDLike t this) => Java this (Iterator t)

foreign import java unsafe top :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe top2 :: (t <: Object, this <: JavaRDDLike t this) => Int -> Comparator t -> Java this (List t)

foreign import java unsafe treeAggregate :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
                                         => u -> Function2 u t u -> Function2 u u u -> Java this u

foreign import java unsafe treeAggregate2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
                                        => u -> Function2 u t u -> Function2 u u u -> Int -> Java this u

foreign import java unsafe treeReduce :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Java this t

foreign import java unsafe treeReduce2 :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Int -> Java this t

foreign import java unsafe wrapRDD :: (t <: Object, this <: JavaRDDLike t this) => RDD t -> Java this this

foreign import java unsafe zip :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => JavaRDDLike u b -> Java this (JavaPairRDD t u)

foreign import java unsafe zipPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object, v <: Object)
                                         => JavaRDDLike u b -> FlatMapFunction2 (Iterator t) (Iterator u) v -> Java this (JavaPairRDD t u)

foreign import java unsafe zipWithIndex :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaPairRDD t JLong)

foreign import java unsafe zipWithUniqueId :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaPairRDD t JLong)
