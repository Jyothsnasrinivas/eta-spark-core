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

data {-# CLASS "org.apache.spark.sql.KeyValueGroupedDataset" #-} KeyValueGroupedDataset k v = KeyValueGroupedDataset (Object# (KeyValueGroupedDataset k v))
  deriving Class

data {-# CLASS "org.apache.spark.sql.DataFrameNaFunctions" #-} DataFrameNaFunctions = DataFrameNaFunctions (Object# DataFrameNaFunctions)
  deriving Class

data {-# CLASS "org.apache.spark.sql.SparkSession" #-} SparkSession = SparkSession (Object# SparkSession)
  deriving Class

data {-# CLASS "org.apache.spark.sql.catalyst.plans.logical.LogicalPlan" #-} LogicalPlan = LogicalPlan (Object# LogicalPlan)
  deriving Class

data {-# CLASS "org.apache.spark.sql.execution.QueryExecution" #-} QueryExecution = QueryExecution (Object# QueryExecution)
  deriving Class

data {-# CLASS "org.apache.spark.sql.types.StructType" #-} StructType = StructType (Object# StructType)
  deriving Class

data {-# CLASS "org.apache.spark.sql.SQLContext" #-} SQLContext = SQLContext (Object# SQLContext)
  deriving Class

data {-# CLASS "org.apache.spark.sql.DataFrameStatFunctions" #-} DataFrameStatFunctions = DataFrameStatFunctions (Object# DataFrameStatFunctions)
  deriving Class

data {-# CLASS "org.apache.spark.sql.DataFrameWriter" #-} DataFrameWriter t = DataFrameWriter (Object# (DataFrameWriter t))
  deriving Class
