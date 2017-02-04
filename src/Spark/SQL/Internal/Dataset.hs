{-# LANGUAGE MagicHash #-}
module Spark.SQL.Internal.Dataset where

import Java

data {-# CLASS "org.apache.spark.api.java.Dataset" #-} Dataset t = Dataset (Object# (Dataset t)
  deriving Class

foreign import java unsafe agg :: (t <: Object) => Column -> ... --Todo
                               ->Java (Dataset t) Dataset(row)

foreign import java unsafe agg2 :: (t <: Object) => Column -> Seq Column
                                ->Java (Dataset t) (Dataset row)

foreign import java unsafe agg3 :: (t <: Object) => Map JString JString
                                ->Java (Dataset t) (Dataset row)

foreign import java unsafe agg4 :: (t <: Object) => Map JString JString
                                ->Java (Dataset t) (Dataset row)

foreign import java unsafe agg5 :: (t <: Object) => Tuple2 JString JString -> Seq (Tuple2 JString JString)
                                -> Java (Dataset t) (Dataset row)

foreign import java unsafe alias :: (t <: Object) => JString
                              -> Java (Dataset t) (Dataset t)

foreign import java unsafe alias2 :: (t <: Object) => Symbol
                                 -> Java (Dataset t) (Dataset t)

foreign import java unsafe apply :: (t <: Object) => JString
                                -> Java (Dataset t) (Column)
      
