module Spark.Core.JavaRDDLike
  ( module X
  , module Spark.Core.JavaRDDLike )
where

import Java
import Spark.Core.Internal.Types
import Spark.Core.Internal.JavaRDDLike as X hiding
  (aggregateByKey
  ,collectPartitions
  ,flatMap
  ,flatMapToDouble
  ,flatMapToPair
  ,fold
  ,foreach
  ,foreachAsync
  ,foreachPartition
  ,foreachPartitionAsync
  ,groupBy
  ,groupBy2
  ,keyBy
  ,map
  ,mapPartitions
  ,mapPartitions2
  ,mapPartitionsToDouble
  ,mapPartitionsToDouble2
  ,mapPartitionsToPair
  ,mapPartitionsToPair2
  ,mapPartitionsWithIndex
  ,mapToDouble
  ,mapToPair
  ,reduce
  ,treeAggregate
  ,treeAggregate2
  ,treeReduce
  ,treeReduce2
  ,zipPartitions)
import qualified Spark.Core.Internal.JavaRDDLike as S

aggregateByKey :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => u
                                          -> (forall a. u -> t -> Java a u)
                                          -> (forall a. u -> u -> Java a u)
                                          -> Java this (u)
aggregateByKey t1 t2 t3 = S.aggregateByKey t1 (mkFun2 t2) (mkFun2 t3)


collectPartitions :: forall t this  . (t <: Object, this <: JavaRDDLike t this) => [Int] -> Java this [[t]]
collectPartitions t = fmap (Prelude.map fromJava . (fromJava :: ListArray t -> [List t])) $ S.collectPartitions (toJava t)

flatMap :: (t <: Object, this <: JavaRDDLike t this, u <: Object) => (forall a. t -> Java a (Iterator u)) -> Java this (JavaRDD u)
flatMap t = S.flatMap (mkFlatMapFun t)

flatMapToDouble :: (t <: Object, this <: JavaRDDLike t this)
                => (forall a. t -> Java a (Iterator JDouble))
                -> Java this JavaDoubleRDD
flatMapToDouble t = S.flatMapToDouble (mkDoubleFlatMapFun t)

flatMapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
              => (forall a. t -> Java a (Iterator (Tuple2 k2 v2)))
              -> Java this (JavaPairRDD k2 v2)
flatMapToPair t = S.flatMapToPair (mkPairFlatMapFun t)

fold :: (t <: Object, this <: JavaRDDLike t this)
     => t -> (forall a. t -> t -> Java a t)
     -> Java this t
fold t1 t2 = S.fold t1 (mkFun2 t2)

foreach :: (t <: Object, this <: JavaRDDLike t this) => (forall a. t -> Java a ()) -> Java this ()
foreach t = S.foreach (mkVoidFun t)

foreachAsync :: (t <: Object, this <: JavaRDDLike t this) => (forall a. t -> Java a ()) -> Java this (JavaFutureAction ())
foreachAsync t = S.foreachAsync (mkVoidFun t)

foreachPartition :: (t <: Object, this <: JavaRDDLike t this) => (forall a. (Iterator t) -> Java a ()) -> Java this ()
foreachPartition t = S.foreachPartition (mkVoidFun t)

foreachPartitionAsync :: (t <: Object, this <: JavaRDDLike t this)
                      => (forall a. (Iterator t) -> Java a ())
                      -> Java this (JavaFutureAction ())
foreachPartitionAsync t = S.foreachPartitionAsync (mkVoidFun t)

groupBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
        => (forall a. t -> Java a u)
        -> Java this (JavaPairRDD u (Iterable t))
groupBy t = S.groupBy (mkFun t)

groupBy2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
        => (forall a. t -> Java a u)
        -> Int
        -> Java this (JavaPairRDD u (Iterable t))
groupBy2 t1 t2 = S.groupBy2 (mkFun t1) t2

keyBy :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
      => (forall a. t -> Java a u)
      -> Java this (JavaPairRDD u t)
keyBy t = S.keyBy (mkFun t)

map :: (t <: Object, this <: JavaRDDLike t this, r <: Object)
    => (forall a. t -> Java a r)
    -> Java this (JavaRDD r)
map t = S.map (mkFun t)

mapPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
              => (forall a. Iterator t -> Java a (Iterator u))
              -> Java this (JavaRDD u)
mapPartitions t = S.mapPartitions (mkFlatMapFun t)

mapPartitions2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
              => (forall a. Iterator t -> Java a (Iterator u))
              -> Bool
              -> Java this (JavaRDD u)
mapPartitions2 t1 t2 = S.mapPartitions2 (mkFlatMapFun t1) t2

mapPartitionsToDouble :: (t <: Object, this <: JavaRDDLike t this)
              => (forall a. Iterator t -> Java a (Iterator JDouble))
              -> Java this (JavaDoubleRDD)
mapPartitionsToDouble t = S.mapPartitionsToDouble (mkDoubleFlatMapFun t)

mapPartitionsToDouble2 :: (t <: Object, this <: JavaRDDLike t this)
              => (forall a. Iterator t -> Java a (Iterator JDouble))
              -> Bool
              -> Java this (JavaDoubleRDD)
mapPartitionsToDouble2 t1 t2 = S.mapPartitionsToDouble2 (mkDoubleFlatMapFun t1) t2

mapPartitionsToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                    => (forall a. Iterator t -> Java a (Iterator (Tuple2 k2 v2)))
                    -> Java this (JavaPairRDD k2 v2)
mapPartitionsToPair t = S.mapPartitionsToPair (mkPairFlatMapFun t)

mapPartitionsToPair2 :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
                    => (forall a. Iterator t -> Java a (Iterator (Tuple2 k2 v2)))
                    -> Bool
                    -> Java this (JavaPairRDD k2 v2)
mapPartitionsToPair2 t1 t2 = S.mapPartitionsToPair2 (mkPairFlatMapFun t1) t2


mapPartitionsWithIndex :: (t <: Object, this <: JavaRDDLike t this, r <: Object)
                    => (forall a. JInteger -> Iterator t -> Java a (Iterator r))
                    -> Bool
                    -> Java this (JavaRDD r)
mapPartitionsWithIndex t1 t2 = S.mapPartitionsWithIndex (mkFun2 t1) t2

mapToDouble :: (t <: Object, this <: JavaRDDLike t this)
            => (forall a. t -> Java a (Iterator JDouble))
            -> Java this JavaDoubleRDD
mapToDouble t = S.mapToDouble (mkDoubleFun t)

mapToPair :: (t <: Object, this <: JavaRDDLike t this, k2 <: Object, v2 <: Object)
          => (forall a. t -> Java a (Tuple2 k2 v2))
          -> Java this (JavaPairRDD k2 v2)
mapToPair t = S.mapToPair (mkPairFun t)

reduce :: (t <: Object, this <: JavaRDDLike t this)
       => (forall a. t -> t -> Java a t)
       -> Java this t
reduce t = S.reduce (mkFun2 t)

treeAggregate :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
              => u -> (forall a. u -> t -> Java a u)
              -> (forall a. u -> u -> Java a u)
              -> Java this u
treeAggregate t1 t2 t3 = S.treeAggregate t1 (mkFun2 t2) (mkFun2 t3)

treeAggregate2 :: (t <: Object, this <: JavaRDDLike t this, u <: Object)
              => u -> (forall a. u -> t -> Java a u)
              -> (forall a. u -> u -> Java a u)
              -> Int
              -> Java this u
treeAggregate2 t1 t2 t3 t4 = S.treeAggregate2 t1 (mkFun2 t2) (mkFun2 t3) t4

treeReduce :: (t <: Object, this <: JavaRDDLike t this) => (forall a. t -> t -> Java a t) -> Java this t
treeReduce t = S.treeReduce (mkFun2 t)

treeReduce2 :: (t <: Object, this <: JavaRDDLike t this) => (forall a. t -> t -> Java a t) -> Int -> Java this t
treeReduce2 t1 t2 = S.treeReduce2 (mkFun2 t1) t2

zipPartitions :: (t <: Object, this <: JavaRDDLike t this, u <: Object, v <: Object)
              => JavaRDDLike u b
              -> (forall a. Iterator t -> Iterator u -> Java a (Iterator v))
              -> Java this (JavaPairRDD t u)
zipPartitions t1 t2 = S.zipPartitions t1 (mkFlatMapFun2 t2)
