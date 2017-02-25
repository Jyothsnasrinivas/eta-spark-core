{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.Types
  (module Spark.Core.Internal.Types,
  module Scala)
where

import Java
import Scala

data {-# CLASS "java.util.List[]" #-} ListArray t = ListArray (Object# (ListArray t))
  deriving Class

instance JArray (List t) (ListArray t)

data {-# CLASS "scala.Tuple2[]" #-} Tuple2Array t1 t2 = Tuple2Array (Object# (Tuple2Array t1 t2))
  deriving Class

instance JArray (Tuple2 t1 t2) (Tuple2Array t1 t2)

data {-# CLASS "org.apache.spark.api.java.JavaRDD" #-} JavaRDD t = JavaRDD (Object# (JavaRDD t))
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaHadoopRDD" #-} JavaHadoopRDD k v = JavaHadoopRDD (Object# (JavaHadoopRDD k v))
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaNewHadoopRDD" #-} JavaNewHadoopRDD k v = JavaNewHadoopRDD (Object# (JavaNewHadoopRDD k v))
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaPairRDD" #-} JavaPairRDD k v = JavaPairRDD (Object# (JavaPairRDD k v))
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaRDDLike" #-} JavaRDDLike t this = JavaRDDLike (Object# (JavaRDDLike t this))
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaDoubleRDD" #-} JavaDoubleRDD = JavaDoubleRDD (Object# JavaDoubleRDD)
  deriving Class

data {-# CLASS "org.apache.spark.api.java.JavaRDD[]" #-} JavaRDDArray t = JavaRDDArray (Object# (JavaRDDArray t))
  deriving Class

instance JArray (JavaRDD t) (JavaRDDArray t)

data {-# CLASS "org.apache.spark.rdd.RDD" #-} RDD t = RDD (Object# (RDD t))
  deriving Class

data {-# CLASS "org.apache.spark.storage.StorageLevel" #-} StorageLevel = StorageLevel (Object# StorageLevel)
  deriving Class

foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.NONE" nONE :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.DISK_ONLY" dISK_ONLY :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.DISK_ONLY_2" dISK_ONLY_2 :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_ONLY" mEMORY_ONLY :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_ONLY_2" mEMORY_ONLY_2 :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_ONLY_SER" mEMORY_ONLY_SER :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_ONLY_SER_2" mEMORY_ONLY_SER_2 :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK" mEMORY_AND_DISK :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK_2" mEMORY_AND_DISK_2 :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK_SER" mEMORY_AND_DISK_SER :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK_SER_2" mEMORY_AND_DISK_SER_2 :: StorageLevel
foreign import java unsafe "@static org.apache.spark.storage.StorageLevel.OFF_HEAP" oFF_HEAP :: StorageLevel

data {-# CLASS "org.apache.spark.api.java.JavaFutureAction" #-} JavaFutureAction t = JavaFutureAction (Object# (JavaFutureAction t))
  deriving Class

foreign import java unsafe "@interface jobIds" jobIds :: Java (JavaFutureAction t) (List JInteger)

data {-# CLASS "org.apache.spark.SparkContext" #-} SparkContext = SparkContext (Object# SparkContext)
  deriving Class

data {-# CLASS "org.apache.spark.partial.PartialResult" #-} PartialResult r = PartialResult (Object# (PartialResult r))
  deriving Class

data {-# CLASS "org.apache.spark.partial.BoundedDouble" #-} BoundedDouble = BoundedDouble (Object# BoundedDouble)
  deriving Class

data {-# CLASS "org.apache.spark.api.java.Optional" #-} Optional t = Optional (Object# (Optional t))
  deriving Class

data {-# CLASS "org.apache.spark.Partition" #-} Partition = Partition (Object# Partition)
  deriving Class

data {-# CLASS "org.apache.spark.TaskContext" #-} TaskContext = TaskContext (Object# TaskContext)
  deriving Class

data {-# CLASS "java.util.Comparator" #-} Comparator t = Comparator (Object# (Comparator t))
  deriving Class

data {-# CLASS "org.apache.spark.Partitioner" #-} Partitioner = Partitioner (Object# Partitioner)
  deriving Class

data {-# CLASS "org.apache.hadoop.io.compress.CompressionCodec" #-} CompressionCodec = CompressionCodec (Object# CompressionCodec)
  deriving Class

data {-# CLASS "org.apache.spark.util.StatCounter" #-} StatCounter = StatCounter (Object# StatCounter)
  deriving Class

data {-# CLASS "org.apache.spark.api.java.function.CoGroupFunction" #-}
  CoGroupFunction k v1 v2 r = CoGroupFunction (Object# (CoGroupFunction k v1 v2 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkCoGroupFun
              :: (k <: Object, v1 <: Object, v2 <: Object, r <: Object)
              => (k -> Iterator v1 -> Iterator v2 -> Java (CoGroupFunction k v1 v2 r) (Iterator r))
              -> CoGroupFunction k v1 v2 r

data {-# CLASS "org.apache.spark.api.java.function.DoubleFlatMapFunction" #-}
  DoubleFlatMapFunction t = DoubleFlatMapFunction (Object# (DoubleFlatMapFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkDoubleFlatMapFun
              :: (t <: Object)
              => (t -> Java (DoubleFlatMapFunction t) (Iterator JDouble))
              -> DoubleFlatMapFunction t

data {-# CLASS "org.apache.spark.api.java.function.DoubleFunction" #-}
  DoubleFunction t = DoubleFunction (Object# (DoubleFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkDoubleFun
              :: (t <: Object)
              => (t -> Java (DoubleFunction t) (Iterator JDouble))
              -> DoubleFunction t

data {-# CLASS "org.apache.spark.api.java.function.FilterFunction" #-}
  FilterFunction t = FilterFunction (Object# (FilterFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkFilterFun
              :: (t <: Object)
              => (t -> Java (FilterFunction t) (Bool))
              -> FilterFunction t

data {-# CLASS "org.apache.spark.api.java.function.FlatMapFunction" #-}
  FlatMapFunction t r = FlatMapFunction (Object# (FlatMapFunction t r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFlatMapFun
              :: (t <: Object, r <: Object)
              => (t -> Java (FlatMapFunction t r) (Iterator r))
              -> FlatMapFunction t r

data {-# CLASS "org.apache.spark.api.java.function.FlatMapFunction2" #-}
  FlatMapFunction2 t1 t2 r = FlatMapFunction2 (Object# (FlatMapFunction2 t1 t2 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFlatMapFun2
              :: (t1 <: Object, t2 <: Object, r <: Object)
              => (t1 -> t2 -> Java (FlatMapFunction2 t1 t2 r) (Iterator r))
              -> FlatMapFunction2 t1 t2 r

data {-# CLASS "org.apache.spark.api.java.function.FlatMapGroupsFunction" #-}
  FlatMapGroupsFunction k v r = FlatMapGroupsFunction (Object# (FlatMapGroupsFunction k v r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFlatMapGroupsFun
              :: (k <: Object, v <: Object, r <: Object)
              => (k -> Iterator v -> Java (FlatMapGroupsFunction k v r) (Iterator r))
              -> FlatMapGroupsFunction k v r

data {-# CLASS "org.apache.spark.api.java.function.ForeachFunction" #-}
  ForeachFunction t = ForeachFunction (Object# (ForeachFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkForeachFun
              :: (t <: Object)
              => (t -> Java (ForeachFunction t) ())
              -> ForeachFunction t

data {-# CLASS "org.apache.spark.api.java.function.ForeachPartitionFunction" #-}
  ForeachPartitionFunction t = ForeachPartitionFunction (Object# (ForeachPartitionFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkForeachPartitionFun
              :: (t <: Object)
              => (t -> Java (ForeachPartitionFunction t) ())
              -> ForeachPartitionFunction t

data {-# CLASS "org.apache.spark.api.java.function.Function" #-}
  Function t1 r = Function (Object# (Function t1 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFun
              :: (t1 <: Object, r <: Object)
              => (t1 -> Java (Function t1 r) r)
              -> Function t1 r

data {-# CLASS "org.apache.spark.api.java.function.Function0" #-}
  Function0 r = Function0 (Object# (Function0 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFun0 :: (r <: Object) => Java (Function0 r) r -> Function0 r

data {-# CLASS "org.apache.spark.api.java.function.Function2" #-}
  Function2 t1 t2 r = Function2 (Object# (Function2 t1 t2 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFun2
              :: (t1 <: Object, t2 <: Object, r <: Object)
              => (t1 -> t2 -> Java (Function2 t1 t2 r) r)
              -> Function2 t1 t2 r

data {-# CLASS "org.apache.spark.api.java.function.Function3" #-}
  Function3 t1 t2 t3 r = Function3 (Object# (Function3 t1 t2 t3 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFun3
              :: (t1 <: Object, t2 <: Object, t3 <: Object, r <: Object)
              => (t1 -> t2 -> t3 -> Java (Function3 t1 t2 t3 r) r)
              -> Function3 t1 t2 t3 r

data {-# CLASS "org.apache.spark.api.java.function.Function4" #-}
  Function4 t1 t2 t3 t4 r = Function4 (Object# (Function4 t1 t2 t3 t4 r))
  deriving Class

foreign import java unsafe "@wrapper call" mkFun4
              :: (t1 <: Object, t2 <: Object, t3 <: Object, t4 <: Object, r <: Object)
              => (t1 -> t2 -> t3 -> t4 -> Java (Function4 t1 t2 t3 t4 r) r)
              -> Function4 t1 t2 t3 t4 r

data {-# CLASS "org.apache.spark.api.java.function.MapFunction" #-}
  MapFunction t u = MapFunction (Object# (MapFunction t u))
  deriving Class

foreign import java unsafe "@wrapper call" mkMapFun
              :: (t <: Object, u <: Object)
              => (t -> Java (MapFunction t u) u)
              -> MapFunction t u

data {-# CLASS "org.apache.spark.api.java.function.MapGroupsFunction" #-}
  MapGroupsFunction k v r = MapGroupsFunction (Object# (MapGroupsFunction k v r))
  deriving Class

foreign import java unsafe "@wrapper call" mkMapGroupsFun
              :: (k <: Object, v <: Object, r <: Object)
              => (k -> Iterator v -> Java (MapGroupsFunction k v r) r)
              -> MapGroupsFunction k v r

data {-# CLASS "org.apache.spark.api.java.function.MapPartitionsFunction" #-}
  MapPartitionsFunction t u = MapPartitionsFunction (Object# (MapPartitionsFunction t u))
  deriving Class

foreign import java unsafe "@wrapper call" mkMapPartitionsFun
              :: (t <: Object, u <: Object)
              => (t -> Java (MapPartitionsFunction t u) (Iterator u))
              -> MapPartitionsFunction t u

data {-# CLASS "org.apache.spark.api.java.function.PairFlatMapFunction" #-}
  PairFlatMapFunction t k v = PairFlatMapFunction (Object# (PairFlatMapFunction t k v))
  deriving Class

foreign import java unsafe "@wrapper call" mkPairFlatMapFun
              :: (t <: Object, k <: Object, v <: Object)
              => (t -> Java (PairFlatMapFunction t k v) (Iterator (Tuple2 k v)))
              -> PairFlatMapFunction t k v

data {-# CLASS "org.apache.spark.api.java.function.PairFunction" #-}
  PairFunction t k v = PairFunction (Object# (PairFunction t k v))
  deriving Class

foreign import java unsafe "@wrapper call" mkPairFun
              :: (t <: Object, k <: Object, v <: Object)
              => (t -> Java (PairFunction t k v) (Tuple2 k v))
              -> PairFunction t k v

data {-# CLASS "org.apache.spark.api.java.function.ReduceFunction" #-}
  ReduceFunction t = ReduceFunction (Object# (ReduceFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkReduceFun
              :: (t <: Object)
              => (t -> t -> Java (ReduceFunction t) t)
              -> ReduceFunction t

data {-# CLASS "org.apache.spark.api.java.function.VoidFunction" #-}
  VoidFunction t = VoidFunction (Object# (VoidFunction t))
  deriving Class

foreign import java unsafe "@wrapper call" mkVoidFun
              :: (t <: Object)
              => (t -> Java (VoidFunction t) ())
              -> VoidFunction t

data {-# CLASS "org.apache.spark.api.java.function.VoidFunction2" #-}
  VoidFunction2 t1 t2 = VoidFunction2 (Object# (VoidFunction2 t1 t2))
  deriving Class

foreign import java unsafe "@wrapper call" mkVoidFun2
              :: (t1 <: Object, t2 <: Object)
              => (t1 -> t2 -> Java (VoidFunction2 t1 t2) ())
              -> VoidFunction2 t1 t2

data {-# CLASS "org.apache.spark.serializer.Serializer" #-} Serializer = Serializer (Object# Serializer)
  deriving Class

data {-# CLASS "org.apache.hadoop.mapred.JobConf" #-} JobConf = JobConf (Object# JobConf)
  deriving Class

data {-# CLASS "org.apache.hadoop.mapreduce.InputSplit" #-} NewInputSplit = NewInputSplit (Object# NewInputSplit)
  deriving Class

data {-# CLASS "org.apache.hadoop.mapred.InputSplit" #-} InputSplit = InputSplit (Object# InputSplit)
  deriving Class
