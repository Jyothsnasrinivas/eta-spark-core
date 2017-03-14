module Spark.Core.Internal.JavaRDDLike where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe aggregateByKey :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => u
                                          -> Function2 u t u -> Function2 u u u -> Java this (u)

foreign import java unsafe cartesian :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => JavaRDDLike u b
                                          -> Java this (JavaRDDLike t u)

foreign import java unsafe checkpoint :: (t <: Object, this <: JavaRDDLike t this) => Java this ()

-- foreign import java unsafe classTag :: (t <: Object, this <: JavaRDDLike t this) => Java this (ClassTag t) --Todo

foreign import java unsafe "@interface collect" collect :: (t <: Object, this <: JavaRDDLike t this) => Java this (List t)

foreign import java unsafe "@interface collectAsync" collectAsync :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaFutureAction (List t))

foreign import java unsafe "@interface collectPartitions" collectPartitions :: (t <: Object, this <: JavaRDDLike t this) => JIntArray -> Java this (ListArray t)

foreign import java unsafe "@interface context" context :: (t <: Object, this <: JavaRDDLike t this) => Java this SparkContext

foreign import java safe "@interface count" count :: (t <: Object, this <: JavaRDDLike t this) => Java this Int64

foreign import java unsafe "@interface countApprox" countApprox :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Java this (PartialResult BoundedDouble)

foreign import java unsafe "@interface countApprox" countApprox2 :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Double -> Java this (PartialResult BoundedDouble)

foreign import java unsafe "@interface countApproxDistinct" countApproxDistinct :: (t <: Object, this <: JavaRDDLike t this) => Double -> Java this (Int64)

foreign import java unsafe "@interface countAsync" countAsync :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaFutureAction Int64)

foreign import java unsafe "@interface countByValue" countByValue :: (t <: Object, this <: JavaRDDLike t this) => Java this (Map t Int64)

foreign import java unsafe "@interface countByValueApprox" countByValueApprox :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Java this (PartialResult (Map t BoundedDouble))

foreign import java unsafe "@interface countByValueApprox2" countByValueApprox2 :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Double -> Java this (PartialResult (Map t BoundedDouble))

foreign import java unsafe "@interface first" first:: (t <: Object, this <: JavaRDDLike t this) => Java this t

foreign import java unsafe "@interface flatMap" flatMap :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => FlatMapFunction t u -> Java this (JavaRDD u)

foreign import java unsafe "@interface flatMapToDouble" flatMapToDouble :: (t <: Object, this <: JavaRDDLike t this) => DoubleFlatMapFunction t -> Java this JavaDoubleRDD

foreign import java unsafe "@interface flatMapToPair" flatMapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object) => PairFlatMapFunction t k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe "@interface fold" fold :: (t <: Object, this <: JavaRDDLike t this) => t -> Function2 t t t -> Java this t

foreign import java unsafe "@interface foreach" foreach :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction t -> Java this ()

foreign import java unsafe "@interface foreachAsync" foreachAsync :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction t -> Java this (JavaFutureAction ())

foreign import java unsafe "@interface foreachPartition" foreachPartition :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction (Iterator t) -> Java this ()

foreign import java unsafe "@interface foreachPartitionAsync" foreachPartitionAsync :: (t <: Object, this <: JavaRDDLike t this) => VoidFunction (Iterator t) -> Java this (JavaFutureAction ())

foreign import java unsafe "@interface getCheckpointFile" getCheckpointFile :: (t <: Object, this <: JavaRDDLike t this) => Java this (Optional JString)

foreign import java unsafe "@interface getNumParitions" getNumParitions :: (t <: Object, this <: JavaRDDLike t this) => Java this Int

foreign import java unsafe "@interface getStorageLevel" getStorageLevel :: (t <: Object, this <: JavaRDDLike t this) => Java this StorageLevel

foreign import java unsafe "@interface glom" glom :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaRDD (List t))

foreign import java unsafe "@interface groupBy" groupBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Java this (JavaPairRDD u (Iterable t))

foreign import java unsafe "@interface groupBy" groupBy2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Int -> Java this (JavaPairRDD u (Iterable t))

foreign import java unsafe "@interface id" id :: (t <: Object, this <: JavaRDDLike t this) => Java this Int

foreign import java unsafe "@interface isCheckpointed" isCheckpointed :: (t <: Object, this <: JavaRDDLike t this) => Java this Bool

foreign import java unsafe "@interface isEmpty" isEmpty :: (t <: Object, this <: JavaRDDLike t this) => Java this Bool

foreign import java unsafe "@interface iterator" iterator :: (t <: Object, this <: JavaRDDLike t this) => Partition -> TaskContext -> Java this (Iterator t)

foreign import java unsafe "@interface keyBy" keyBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => Function t u -> Java this (JavaPairRDD u t)

foreign import java unsafe "@interface map" map :: (t <: Object, this <: JavaRDDLike t this, r <: Object) => Function t r -> Java this (JavaRDD r)

foreign import java unsafe "@interface mapPartitions" mapPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
                                         => FlatMapFunction (Iterator t) u -> Java this (JavaRDD u)

foreign import java unsafe "@interface mapPartitions" mapPartitions2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => FlatMapFunction (Iterator t) u -> Bool -> Java this (JavaRDD u)

foreign import java unsafe "@interface mapPartitionsToDouble" mapPartitionsToDouble :: (t <: Object, this <: JavaRDDLike t this) => DoubleFlatMapFunction (Iterator t) -> Java this JavaDoubleRDD

foreign import java unsafe "@interface mapPartitionsToDouble" mapPartitionsToDouble2 :: (t <: Object, this <: JavaRDDLike t this)
                                                  => DoubleFlatMapFunction (Iterator t) -> Bool -> Java this JavaDoubleRDD

foreign import java unsafe "@interface mapPartitionsToPair" mapPartitionsToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                              => PairFlatMapFunction (Iterator t) k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe "@interface mapPartitionsToPair" mapPartitionsToPair2 :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                               => PairFlatMapFunction (Iterator t) k2 v2 -> Bool -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe "@interface mapPartitionsWithIndex" mapPartitionsWithIndex :: (t <: Object, this <: JavaRDDLike t this, r <: Object)
                                                  => Function2 JInteger (Iterator t) (Iterator r) -> Bool -> Java this (JavaRDD r)

foreign import java unsafe "@interface mapToDouble" mapToDouble :: (t <: Object, this <: JavaRDDLike t this) => DoubleFunction t -> Java this JavaDoubleRDD

foreign import java unsafe "@interface mapToDouble" mapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                                     => PairFunction t k2 v2 -> Java this (JavaPairRDD k2 v2)

foreign import java unsafe "@interface max" max :: (t <: Object, this <: JavaRDDLike t this) => Comparator t -> Java this t

foreign import java unsafe "@interface min" min :: (t <: Object, this <: JavaRDDLike t this) => Comparator t -> Java this t

foreign import java unsafe "@interface name" name :: (t <: Object, this <: JavaRDDLike t this) => Java this String

foreign import java unsafe "@interface partitioner" partitioner :: (t <: Object, this <: JavaRDDLike t this) => Java this (Optional Partitioner)

foreign import java unsafe "@interface partitions" partitions :: (t <: Object, this <: JavaRDDLike t this) => Java this (List Partition)

foreign import java unsafe "@interface pipe" pipe :: (t <: Object, this <: JavaRDDLike t this) => List JString -> Java this (JavaRDD JString)

foreign import java unsafe "@interface pipe" pipe2 :: (t <: Object, this <: JavaRDDLike t this) => List JString -> Map JString JString -> Java this (JavaRDD JString)

foreign import java unsafe "@interface pipe" pipe3 :: (t <: Object, this <: JavaRDDLike t this)
                                 => List JString -> Map JString JString -> Bool -> Int -> Java this (JavaRDD JString)

foreign import java unsafe "@interface pipe" pipe4 :: (t <: Object, this <: JavaRDDLike t this)
                                => List JString -> Map JString JString -> Bool -> Int -> String -> Java this (JavaRDD JString)

foreign import java unsafe "@interface pipe" pipe5 :: (t <: Object, this <: JavaRDDLike t this) => String -> Java this (JavaRDD JString)

foreign import java unsafe "@interface rdd" rdd :: (t <: Object, this <: JavaRDDLike t this) => Java this (RDD t)

foreign import java unsafe "@interface reduce" reduce :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Java this t

foreign import java unsafe "@interface saveAsObjectFile" saveAsObjectFile :: (t <: Object, this <: JavaRDDLike t this) => String -> Java this ()

foreign import java unsafe "@interface saveAsTextFile" saveAsTextFile :: (t <: Object, this <: JavaRDDLike t this) => String -> Java this ()

foreign import java unsafe "saveAsTextFile" saveAsTextFile2 :: (t <: Object, this <: JavaRDDLike t this, b <: CompressionCodec) => String -> JClass b -> Java this ()

foreign import java unsafe "@interface take" take :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe "@interface takeAsync" takeAsync :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (JavaFutureAction (List t))

foreign import java unsafe "@interface takeOrdered" takeOrdered :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe "@interface takeOrdered" takeOrdered2 :: (t <: Object, this <: JavaRDDLike t this) => Int -> Comparator t -> Java this (List t)

foreign import java unsafe "@interface takeSample" takeSample :: (t <: Object, this <: JavaRDDLike t this) => Bool -> Int -> Java this (List t)

foreign import java unsafe "@interface takeSample" takeSample2 :: (t <: Object, this <: JavaRDDLike t this) => Bool -> Int -> Int64 -> Java this (List t)

foreign import java unsafe "@interface toDebugString" toDebugString :: (t <: Object, this <: JavaRDDLike t this) => Java this String

foreign import java unsafe "@interface toLocalIterator" toLocalIterator :: (t <: Object, this <: JavaRDDLike t this) => Java this (Iterator t)

foreign import java unsafe "@interface top" top :: (t <: Object, this <: JavaRDDLike t this) => Int -> Java this (List t)

foreign import java unsafe "@interface top" top2 :: (t <: Object, this <: JavaRDDLike t this) => Int -> Comparator t -> Java this (List t)

foreign import java unsafe "@interface treeAggregate" treeAggregate :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
                                         => u -> Function2 u t u -> Function2 u u u -> Java this u

foreign import java unsafe "@interface treeAggregate" treeAggregate2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
                                        => u -> Function2 u t u -> Function2 u u u -> Int -> Java this u

foreign import java unsafe "@interface treeReduce" treeReduce :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Java this t

foreign import java unsafe "@interface treeReduce" treeReduce2 :: (t <: Object, this <: JavaRDDLike t this) => Function2 t t t -> Int -> Java this t

foreign import java unsafe "@interface wrapRDD" wrapRDD :: (t <: Object, this <: JavaRDDLike t this) => RDD t -> Java this this

foreign import java unsafe "@interface zip" zip :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => JavaRDDLike u b -> Java this (JavaPairRDD t u)

foreign import java unsafe "@interface zipPartitions" zipPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object, v <: Object)
                                         => JavaRDDLike u b -> FlatMapFunction2 (Iterator t) (Iterator u) v -> Java this (JavaPairRDD t u)

foreign import java unsafe "@interface zipWithIndex" zipWithIndex :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaPairRDD t JLong)

foreign import java unsafe "@interface zipWithUniqueId" zipWithUniqueId :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaPairRDD t JLong)
