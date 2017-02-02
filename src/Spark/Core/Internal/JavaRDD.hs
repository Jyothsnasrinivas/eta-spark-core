{-# LANGUAGE MagicHash #-}
module Test where

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
