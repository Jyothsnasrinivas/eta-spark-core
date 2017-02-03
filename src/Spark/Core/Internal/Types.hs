{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.Types where

import Java

data {-# CLASS "org.apache.spark.api.java.function.CoGroupFunction" #-}
  CoGroupFunction k v1 v2 r = CoGroupFunction (Object# (CoGroupFunction k v1 v2 r)
  deriving Class

foreign import java unsafe "@wrapper call" mkCoGroupFun
              :: (k <: Object, v1 <: Object, v2 <: Object, r <: Object)
              => (k -> Iterator v1 -> Iterator v2 -> Java (CoGroupFunction k v1 v2 r) (Iterator r))
              -> CoGroupFunction k v1 v2 r

data {-# CLASS "org.apache.spark.api.java.function.DoubleFlatMapFunction" #-}
  DoubleFlatMapFunction t = DoubleFlatMapFunction (Object# (DoubleFlatMapFunction t)
  deriving Class

foreign import java unsafe "@wrapper call" mkDoubleFlatMapFun
              :: (t <: Object)
              => (t -> Java (DoubleFlatMapFunction t) (Iterator JDouble))
              -> DoubleFlatMapFunction t

data {-# CLASS "org.apache.spark.api.java.function.DoubleFunction" #-}
  DoubleFunction t = DoubleFunction (Object# (DoubleFunction t)
  deriving Class

foreign import java unsafe "@wrapper call" mkDoubleFun
              :: (t <: Object)
              => (t -> Java (DoubleFunction t) (Iterator Double))
              -> DoubleFunction t

data {-# CLASS "org.apache.spark.api.java.function.FilterFunction" #-}
  FilterFunction t = FilterFunction (Object# (FilterFunction t)
  deriving Class

foreign import java unsafe "@wrapper call" mkFilterFun
              :: (t <: Object)
              => (t -> Java (FilterFunction t) (Bool))
              -> FilterFunction t

data {-# CLASS "org.apache.spark.api.java.function.FlatMapFunction" #-}
  FlatMapFunction t r = FlatMapFunction (Object# (FlatMapFunction t r)
  deriving Class

foreign import java unsafe "@wrapper call" mkFlatMapFun
              :: (t <: Object, r <: Object)
              => (t -> Java (FlatMapFunction t r) (Iterator r))
              -> FlatMapFunction t r

data {-# CLASS "org.apache.spark.api.java.function.FlatMapFunction2" #-}
  FlatMapFunction2 t1 t2 r = FlatMapFunction2 (Object# (FlatMapFunction2 t1 t2 r)
  deriving Class

foreign import java unsafe "@wrapper call" mkFlatMapFun
              :: (t1 <: Object, t2 <: Object, r <: Object)
              => (t1 -> t2 -> Java (FlatMapFunction2 t1 t2 r) (Iterator r))
              -> FlatMapFunction2 t1 t2 r
                    
