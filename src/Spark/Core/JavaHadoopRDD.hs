{-# LANGUAGE MagicHash #-}

module Spark.Core.JavaHadoopRDD where

import Java
import qualified Spark.Core.JavaHadoopRDD as S

data {-# CLASS "org.apache.spark.api.java.JavaHadoopRDD" #-} JavaHadoopRDD k v = JavaHadoopRDD (Object# (JavaHadoopRDD k v)
  deriving Class

foreign import java unsafe kClassTag :: (k <: Object, v <: Object) => Java (JavaHadoopRDD k v) (ClassTag k)

mapPartitionWithInputSplit :: (k <: Object, v <: Object, r <: Object)
                           => (forall a. InputSplit -> Iterator (Tuple2 k v) -> Java a (Iterator r))
                           -> Bool -> Java (JavaHadoopRDD k v) (ClassTag k)--TODO
                           

foreign import java unsafe mapPartitionWithInputSplit :: (k <: Object, v <: Object, r <: Object)
                  => Function2 InputSplit (Iterator (Tuple2 k v)) (Iterator r) -> Bool -> Java (JavaHadoopRDD k v) (ClassTag k)

foreign import java unsafe vClassTag :: (k <: Object, v <: Object) => Java (JavaHadoopRDD k v) (ClassTag v)
