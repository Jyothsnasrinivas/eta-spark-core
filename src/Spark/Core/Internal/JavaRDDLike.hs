{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.JavaRDDLike where

import Java

data {-# CLASS "org.apache.spark.api.java.JavaRDDLike" #-} JavaRDDLike t this = JavaRDDLike (Object# (JavaRDDLike t this)
  deriving Class

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

foreign import java unsafe count :: (t <: Object, this <: JavaRDDLike t this) => Java this (Int64) -- TODO

foreign import java unsafe countApprox :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Java this (PartialResult BoundedDouble)

foreign import java unsafe countApprox2 :: (t <: Object, this <: JavaRDDLike t this) => Int64 -> Double -> Java this (PartialResult BoundedDouble)

foreign import java unsafe countApproxDistinct :: (t <: Object, this <: JavaRDDLike t this) => Double -> Java this (Int64) --TODO

foreign import java unsafe countAsync :: (t <: Object, this <: JavaRDDLike t this) => Java this (JavaFutureAction Long) --TODO

foreign import java unsafe countByValue :: (t <: Object, this <: JavaRDDLike t this) => Java this (Map t Long) --TODO
