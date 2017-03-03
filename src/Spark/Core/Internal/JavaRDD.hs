
module Spark.Core.Internal.JavaRDD where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe cache :: (t <: Object) => Java (JavaRDD t) (JavaRDD t)

-- foreign import java unsafe classTag :: (t <: Object) => Java (JavaRDD t) (ClassTag t)

foreign import java unsafe coalesce :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe coalesce2 :: (t <: Object) => Int -> Bool -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe distinct :: (t <: Object) => Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe distinct2 :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe filter :: (t <: Object) => Function t JBoolean -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe intersection :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe persist :: (t <: Object) => StorageLevel -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe randomSplit :: (t <: Object) => JDoubleArray -> Java (JavaRDD t) (JavaRDDArray t)

foreign import java unsafe randomSplit2 :: (t <: Object) => JDoubleArray -> Int64 -> Java (JavaRDD t) (JavaRDDArray t)

--foreign import java unsafe rdd :: (t <: Object) => Java (JavaRDD t) (RDD t)

foreign import java unsafe repartition :: (t <: Object) => Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sample :: (t <: Object) => Bool -> Double -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sample2 :: (t <: Object) => Bool -> Double -> Int64 -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe setName :: (t <: Object) => String -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe sortBy :: (t <: Object, s <: Object) => Function t s -> Bool -> Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract2 :: (t <: Object) => JavaRDD t -> Int -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe subtract3 :: (t <: Object) => JavaRDD t -> Partitioner -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe union :: (t <: Object) => JavaRDD t -> Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe unpersist :: (t <: Object) =>  Java (JavaRDD t) (JavaRDD t)

foreign import java unsafe unpersist2 :: (t <: Object) => Bool -> Java (JavaRDD t) (JavaRDD t)

--foreign import java unsafe wrapRDD :: (t <: Object) => RDD t -> Java (JavaRDD t) (JavaRDD t)
