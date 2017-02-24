{-# LANGUAGE MagicHash #-}
module Spark.SQL.Internal.Types where

import Java

data {-# CLASS "org.apache.spark.sql.Column[]" #-} ColumnArray = ColumnArray (Object# ColumnArray)
  deriving Class

data {-# CLASS "org.apache.spark.sql.Dataset[]" #-} DatasetArray t = DatasetArray (Object# (DatasetArray t))
  deriving Class
