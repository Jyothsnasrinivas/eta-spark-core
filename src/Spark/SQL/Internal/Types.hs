{-# LANGUAGE MagicHash #-}
module Spark.SQL.Internal.Types
 (module Spark.SQL.Internal.Types,
  module Spark.Core.Internal.Types)
where

import Java
import Spark.Core.Internal.Types

data {-# CLASS "org.apache.spark.sql.Dataset[]" #-} DatasetArray t = DatasetArray (Object# (DatasetArray t))
  deriving Class

data {-# CLASS "org.apache.spark.sql.Column" #-} Column = Column (Object# Column)
  deriving Class

data {-# CLASS "org.apache.spark.sql.Column[]" #-} ColumnArray = ColumnArray (Object# ColumnArray)
  deriving Class

instance JArray Column ColumnArray

data {-# CLASS "org.apache.spark.sql.Encoder" #-} Encoder t = Encoder (Object# (Encoder t))
  deriving Class

data {-# CLASS "org.apache.spark.sql.TypedColumn" #-} TypedColumn t u = TypedColumn (Object# (TypedColumn t u))
  deriving Class

data {-# CLASS "org.apache.spark.sql.types.Metadata" #-} Metadata = Metadata (Object# Metadata)
  deriving Class

data {-# CLASS "org.apache.spark.sql.types.DataType" #-} DataType = DataType (Object# DataType)
  deriving Class

data {-# CLASS "org.apache.spark.sql.Dataset" #-} Dataset t = Dataset (Object# (Dataset t))
  deriving Class

data {-# CLASS "org.apache.spark.sql.Row" #-} Row = Row (Object# Row)
  deriving Class

data {-# CLASS "org.apache.spark.sql.catalyst.expressions.Expression" #-} Expression = Expression (Object# Expression)
  deriving Class

data {-# CLASS "org.apache.spark.sql.expressions.WindowSpec" #-} WindowSpec = WindowSpec (Object# WindowSpec)
  deriving Class

data {-# CLASS "org.apache.spark.sql.RelationalGroupedDataset" #-} RelationalGroupedDataset = RelationalGroupedDataset (Object# RelationalGroupedDataset)
  deriving Class
