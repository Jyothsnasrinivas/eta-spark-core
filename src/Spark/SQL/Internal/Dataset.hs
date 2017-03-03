
module Spark.SQL.Internal.Dataset where

import Java
import Spark.SQL.Internal.Types

foreign import java unsafe agg :: (t <: Object) => Column -> ColumnArray
                               -> Java (Dataset t) (Dataset Row)

-- foreign import java unsafe agg2 :: (t <: Object) => Column -> Seq Column
--                                 -> Java (Dataset t) (Dataset Row)

foreign import java unsafe "agg" agg2 :: (t <: Object) => Map JString JString
                                -> Java (Dataset t) (Dataset Row)

foreign import java unsafe alias :: (t <: Object) => String
                              -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe alias2 :: (t <: Object) => Symbol
--                                  -> Java (Dataset t) (Dataset t)

foreign import java unsafe apply :: (t <: Object) => String
                                -> Java (Dataset t) (Column)

foreign import java unsafe as :: (t <: Object, u <: Object) => Encoder u
                              -> Java (Dataset t) (Dataset u)

foreign import java unsafe "as" as2 :: (t <: Object) => String
                               -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe as3 :: (t <: Object) => Symbol
--                               -> Java (Dataset t) (Dataset t)

foreign import java unsafe cache :: (t <: Object) => Java (Dataset t) (Dataset t)

foreign import java unsafe checkpoint :: (t <: Object) => Java (Dataset t) (Dataset t)

foreign import java unsafe "checkpoint" checkpoint2 :: (t <: Object) => Bool -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe classTag :: (t <: Object) => Java (Dataset t) (ClassTag t)

foreign import java unsafe coalesce :: (t <: Object) => Int -> Java (Dataset t) (Dataset t)

foreign import java unsafe col :: (t <: Object) => String
                               -> Java (Dataset t) (Column)

foreign import java unsafe collect :: (t <: Object) => Java (Dataset t) (Object)

foreign import java unsafe collectAsList :: (t <: Object) => Java (Dataset t) (List t)

foreign import java unsafe columns :: (t <: Object) => Java (Dataset t) (JStringArray)

foreign import java unsafe count :: (t <: Object) => Java (Dataset t) (Int64)

foreign import java unsafe createGlobalTempView :: (t <: Object) => String -> Java (Dataset t) ()

foreign import java unsafe createOrReplaceTempView :: (t <: Object)
                                                   => String -> Java (Dataset t) ()

foreign import java unsafe createTempView :: (t <: Object) => String -> Java (Dataset t) ()

foreign import java unsafe crossJoin :: (t <: Object) => Dataset b -> Java (Dataset t) (Dataset row)

foreign import java unsafe cube:: (t <: Object) => ColumnArray -> Java (Dataset t) (RelationalGroupedDataset)

--foreign import java unsafe cube2 :: (t <: Object) => Seq Column -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe cube3 :: (t <: Object)
--                                  => String -> Seq Column -> Java (Dataset t) (RelationalGroupedDataset)

foreign import java unsafe "cube" cube2 :: (t <: Object)
                                 => String -> JStringArray -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe describe :: (t <: Object) => Seq JString -> Java (Dataset t) (Dataset row)

foreign import java unsafe describe :: (t <: Object) => JStringArray -> Java (Dataset t) (Dataset row)

foreign import java unsafe distinct :: (t <: Object) => Java (Dataset t) (Dataset t)

foreign import java unsafe drop :: (t <: Object) => Column -> Java (Dataset t) (Dataset row)

-- foreign import java unsafe drop2 :: (t <: Object) => Seq JString -> Java (Dataset t) (Dataset row)

foreign import java unsafe "drop" drop2 :: (t <: Object) => JStringArray -> Java (Dataset t) (Dataset row)

foreign import java unsafe "drop" drop3 :: (t <: Object) => String -> Java (Dataset t) (Dataset row)

foreign import java unsafe dropDuplicates :: (t <: Object) => Java (Dataset t) (Dataset t)

-- foreign import java unsafe dropDuplicates2 :: (t <: Object) => Seq JString -> Java (Dataset t) (Dataset t)

foreign import java unsafe "dropDuplicates" dropDuplicates2 :: (t <: Object) => JStringArray -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe dropDuplicates4 :: (t <: Object)
--                                           => JString -> Seq JString -> Java (Dataset t) (Dataset t)

foreign import java unsafe "dropDuplicates" dropDuplicates3 :: (t <: Object)
                                           => String -> JStringArray -> Java (Dataset t) (Dataset t)

foreign import java unsafe dtypes :: (t <: Object) => Java (Dataset t) (Tuple2 JString JString)

foreign import java unsafe except:: (t <: Object) => Dataset t -> Java (Dataset t) (Dataset t)

foreign import java unsafe explain :: (t <: Object) => Java (Dataset t) ()

foreign import java unsafe "explain" explain2 :: (t <: Object) => Bool -> Java (Dataset t) ()

-- foreign import java unsafe explode :: (t <: Object, a <: Product)
--                                   => Seq Column -> Functional (TraversableOnce B) -> Java (Dataset t) (Dataset Row)--TODO
--
-- foreign import java unsafe explode2 :: (t <: Object)
--                                   => JString -> JString -> Functional (TraversableOnce B) -> Java (Dataset t) (Dataset Row) --TODO

foreign import java unsafe filter :: (t <: Object) => Column -> Java (Dataset t) (Dataset t)

foreign import java unsafe "filter" filter2 :: (t <: Object) => FilterFunction t -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe filter3 :: (t <: Object) => Function1 t -> Java (Dataset t) (Dataset t)

foreign import java unsafe "filter" filter3 :: (t <: Object) => String -> Java (Dataset t) (Dataset t)

foreign import java unsafe first :: (t <: Object) => Java (Dataset t) (t)

foreign import java unsafe flatMap :: (t <: Object, u <: Object)
                                    => FlatMapFunction t u -> Encoder u -> Java (Dataset t) (Dataset u)

-- foreign import java unsafe flatMap2 :: (t <: Object, u <: Object)
--                                     => Function1 t (TraversableOnce u) -> Encoder u -> Java (Dataset t) (Dataset u)

foreign import java unsafe foreach :: (t <: Object) => ForeachFunction t -> Java (Dataset t) ()

-- foreign import java unsafe foreach2 :: (t <: Object) => Function1 t BoxedUnit -> Java (Dataset t) ()

foreign import java unsafe foreachPartition :: (t <: Object) => ForeachPartitionFunction t -> Java (Dataset t) ()

-- foreign import java unsafe foreachPartition2 :: (t <: Object)
--                                             => Function1 (Iterator t) BoxedUnit -> Java (Dataset t) ()

foreign import java unsafe groupBy :: (t <: Object) => ColumnArray -> Java (Dataset t) (RelationalGroupedDataset)--TODO

-- foreign import java unsafe groupBy2 :: (t <: Object) => Seq Column -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe groupBy3 :: (t <: Object) => JString -> Seq JString -> Java (Dataset t) (RelationalGroupedDataset)

foreign import java unsafe groupBy2 :: (t <: Object) => String -> JStringArray -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe groupByKey :: (t <: Object, k<: Object)
--                                     => Function1 t k -> Encoder k -> Java (Dataset t) (KeyValueGroupedDataset k t)

foreign import java unsafe groupByKey :: (t <: Object, k<: Object)
                                    => MapFunction t k -> Encoder k -> Java (Dataset t) (KeyValueGroupedDataset k t)

foreign import java unsafe head :: (t <: Object) => Java (Dataset t) (t)

foreign import java unsafe head2 :: (t <: Object) => Int -> Java (Dataset t) (Object)

foreign import java unsafe inputFiles :: (t <: Object) =>  Java (Dataset t) (JStringArray)

foreign import java unsafe intesect :: (t <: Object) => Dataset t -> Java (Dataset t) (Dataset t)

foreign import java unsafe isLocal :: (t <: Object) => Java (Dataset t) (Bool)

foreign import java unsafe isStreaming :: (t <: Object) => Java (Dataset t) (Bool)

foreign import java unsafe javaRDD :: (t <: Object) => Java (Dataset t) (JavaRDD t)

foreign import java unsafe join :: (t <: Object) => Dataset b -> Java (Dataset t) (Dataset row)

foreign import java unsafe "join" join2 :: (t <: Object) => Dataset b -> Column -> Java (Dataset t) (Dataset row)

foreign import java unsafe "join" join3 :: (t <: Object) => Dataset b -> Column -> String -> Java (Dataset t) (Dataset row)

-- foreign import java unsafe join4 :: (t <: Object) => Dataset b -> Seq JString -> Java (Dataset t) (Dataset row)
--
-- foreign import java unsafe join5 :: (t <: Object) => Dataset b -> Seq JString -> JString -> Java (Dataset t) (Dataset row) --TODO

foreign import java unsafe join6 :: (t <: Object) => Dataset b -> String -> Java (Dataset t) (Dataset row)

foreign import java unsafe joinWith :: (t <: Object, u <:Object)
                                    => Dataset u -> Column -> Java (Dataset t) (Dataset (Tuple2 t t))

foreign import java unsafe joinWith2 :: (t <: Object, u <:Object)
                                  => Dataset u -> Column -> String -> Java (Dataset t) (Dataset (Tuple2 t t))

foreign import java unsafe limit :: (t <: Object) => Int -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe map :: (t <: Object, u <: Object)
--                                => Function1 t u -> Encoder u -> Java (Dataset t) (Dataset u)

foreign import java unsafe map :: (t <: Object, u <: Object)
                              => MapFunction t u -> Encoder u -> Java (Dataset t) (Dataset u)

-- foreign import java unsafe mapPartitions :: (t <: Object, u <: Object)
--                                => Function1 (Iterator t) (Iterator u) -> Encoder u -> Java (Dataset t) (Dataset u)

foreign import java unsafe mapPartitions :: (t <: Object, u <: Object)
                              => MapPartitionsFunction t u -> Encoder u -> Java (Dataset t) (Dataset u)

foreign import java unsafe na :: (t <: Object) => Java (Dataset t) (DataFrameNaFunctions)

foreign import java unsafe ofRows :: (t <: Object) => SparkSession -> LogicalPlan -> Java (Dataset t) (Dataset row)

foreign import java unsafe orderBy :: (t <: Object) => ColumnArray -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe orderBy2 :: (t <: Object) => Seq Column -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe orderBy3 :: (t <: Object) => JString -> Seq Column -> Java (Dataset t) (Dataset t)

foreign import java unsafe "orderBy" orderBy2 :: (t <: Object) => String -> JStringArray -> Java (Dataset t) (Dataset t)

foreign import java unsafe persist :: (t <: Object) => Java (Dataset t) (Dataset t)

foreign import java unsafe "persist" persist2 :: (t <: Object) => StorageLevel -> Java (Dataset t) (Dataset t)

foreign import java unsafe printSchema :: (t <: Object) => Java (Dataset t) ()

foreign import java unsafe queryExecution :: (t <: Object) => Java (Dataset t) (QueryExecution)

foreign import java unsafe randomSplit :: (t <: Object) => JDoubleArray -> Java (Dataset t) (DatasetArray t)

foreign import java unsafe randomSplit2 :: (t <: Object) => JDoubleArray -> Int64 -> Java (Dataset t) (DatasetArray t)

foreign import java unsafe randomSplitAsList :: (t <: Object) => JDoubleArray -> Int64 -> Java (Dataset t) (List (Dataset t))

foreign import java unsafe rdd :: (t <: Object) => Java (Dataset t) (RDD t)

foreign import java unsafe reduce :: (t <: Object) => Function2 t t t -> Java (Dataset t) (t)

foreign import java unsafe "reduce" reduce2 :: (t <: Object) => ReduceFunction t -> Java (Dataset t) (t)

foreign import java unsafe registerTempTable :: (t <: Object) => String -> Java (Dataset t) ()

foreign import java unsafe repartition :: (t <: Object) => ColumnArray -> Java (Dataset t) (Dataset t)

foreign import java unsafe "repartition" repartition2 :: (t <: Object) => Int -> Java (Dataset t) (Dataset t)

foreign import java unsafe "repartition" repartition3 :: (t <: Object) => Int -> ColumnArray -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe repartition4 :: (t <: Object) => Int -> Seq Column -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe "repartition" repartition4 :: (t <: Object) => Seq Column -> Java (Dataset t) (Dataset t)

foreign import java unsafe rollup :: (t <: Object) => ColumnArray -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe "rollup" rollup2 :: (t <: Object) => Seq Column -> Java (Dataset t) (RelationalGroupedDataset)

-- foreign import java unsafe "rollup" rollup2 :: (t <: Object) => JString -> Seq Column -> Java (Dataset t) (RelationalGroupedDataset)

foreign import java unsafe "rollup" rollup2 :: (t <: Object) => String -> JStringArray -> Java (Dataset t) (RelationalGroupedDataset)

foreign import java unsafe sample :: (t <: Object) => Bool -> Double -> Java (Dataset t) (Dataset t)

foreign import java unsafe "sample" sample2 :: (t <: Object) => Bool -> Double -> Int64 -> Java (Dataset t) (Dataset t)

foreign import java unsafe "sample" sample3 :: (t <: Object) => Bool -> JDouble -> Java (Dataset t) (Dataset t)

foreign import java unsafe schema :: (t <: Object) => Java (Dataset t) (StructType)

foreign import java unsafe select :: (t <: Object) => ColumnArray -> Java (Dataset t) (Dataset row)

-- foreign import java unsafe select2 :: (t <: Object) => Seq Column -> Java (Dataset t) (Dataset row)

-- foreign import java unsafe select3 :: (t <: Object) => JString -> Seq JString -> Java (Dataset t) (Dataset row)

foreign import java unsafe "select" select2 :: (t <: Object) => String -> JStringArray -> Java (Dataset t) (Dataset row)

foreign import java unsafe "select" select3 :: (t <: Object, u1 <: Object) => TypedColumn t u1 -> Java (Dataset t) (Dataset u1)

foreign import java unsafe "select" select4 :: (t <: Object, u1 <: Object, u2 <: Object)
                                   => TypedColumn t u1 -> TypedColumn t u2 -> Java (Dataset t) (Dataset (Tuple2 u1 u2))

foreign import java unsafe "select" select5 :: (t <: Object, u1 <: Object, u2 <: Object, u3 <: Object)
                                  => TypedColumn t u1 -> TypedColumn t u2 -> TypedColumn t u3 -> Java (Dataset t) (Dataset (Tuple3 u1 u2 u3))

foreign import java unsafe "select" select6 :: (t <: Object, u1 <: Object, u2 <: Object, u3 <: Object, u4 <: Object)
                                   => TypedColumn t u1 -> TypedColumn t u2 -> TypedColumn t u3 -> TypedColumn t u4 -> Java (Dataset t) (Dataset (Tuple4 u1 u2 u3 u4))


foreign import java unsafe "select" select7 :: (t <: Object, u1 <: Object, u2 <: Object, u3 <: Object, u4 <: Object, u5 <: Object)
                                  => TypedColumn t u1 -> TypedColumn t u2 -> TypedColumn t u3 -> TypedColumn t u4 -> TypedColumn t u5 -> Java (Dataset t) (Dataset (Tuple5 u1 u2 u3 u4 u5))

-- foreign import java unsafe selectExpr :: (t <: Object) => Seq JString -> Java (Dataset t) (Dataset row)

foreign import java unsafe selectExpr :: (t <: Object) => JStringArray -> Java (Dataset t) (Dataset row)

foreign import java unsafe show :: (t <: Object) => Java (Dataset t) ()

foreign import java unsafe show2 :: (t <: Object) => Bool -> Java (Dataset t) ()

foreign import java unsafe show3 :: (t <: Object) => Int -> Java (Dataset t) ()

foreign import java unsafe show4 :: (t <: Object) => Int -> Bool -> Java (Dataset t) ()

foreign import java unsafe show5 :: (t <: Object) => Int -> Int -> Java (Dataset t) ()

foreign import java unsafe sort :: (t <: Object) => ColumnArray -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe sort2 :: (t <: Object) => Seq Column -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe sort3 :: (t <: Object) => JString -> Seq JString -> Java (Dataset t) (Dataset t)

foreign import java unsafe "sort" sort2 :: (t <: Object) => String -> JStringArray -> Java (Dataset t) (Dataset t)

foreign import java unsafe sortWithinPartitions :: (t <: Object) => ColumnArray -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe sortWithinPartitions2 :: (t <: Object) => Seq Column -> Java (Dataset t) (Dataset t)

-- foreign import java unsafe sortWithinPartitions3 :: (t <: Object)
--                                                  => JString -> Seq JString -> Java (Dataset t) (Dataset t)

foreign import java unsafe "sortWithinPartitions" sortWithinPartitions2 :: (t <: Object)
                                                 => String -> JStringArray -> Java (Dataset t) (Dataset t)

foreign import java unsafe sparkSession :: (t <: Object) => Java (Dataset t) (SparkSession)

foreign import java unsafe sqlContext :: (t <: Object) => Java (Dataset t) (SQLContext)

foreign import java unsafe stat :: (t <: Object) => Java (Dataset t) (DataFrameStatFunctions)

foreign import java unsafe storageLevel :: (t <: Object) => Java (Dataset t) (StorageLevel)

foreign import java unsafe take :: (t <: Object) => Int -> Java (Dataset t) (Object)

foreign import java unsafe takeAsList :: (t <: Object) => Int -> Java (Dataset t) (List t)

foreign import java unsafe toDF :: (t <: Object) => Java (Dataset t) (Dataset row)

-- foreign import java unsafe toDF2 :: (t <: Object) => Seq JString -> Java (Dataset t) (Dataset row)

foreign import java unsafe "toDF" toDF2 :: (t <: Object) => JStringArray -> Java (Dataset t) (Dataset row)

foreign import java unsafe toJavaRDD :: (t <: Object) => Java (Dataset t) (JavaRDD t)

foreign import java unsafe toJSON :: (t <: Object) => Java (Dataset t) (Dataset JString)

foreign import java unsafe toLocalIterator :: (t <: Object) => Java (Dataset t) (Iterator t)

foreign import java unsafe toString :: (t <: Object) => Java (Dataset t) (JString)

-- foreign import java unsafe transform :: (t <: Object, u <: Object)
--                                      => Function1 (Dataset t) (Dataset u) -> Dataset u -> Java (Dataset t) (Dataset u) --TODO

foreign import java unsafe union :: (t <: Object) => Dataset t -> Java (Dataset t) (Dataset t)

foreign import java unsafe unionAll :: (t <: Object) => Dataset t -> Java (Dataset t) (Dataset t)

foreign import java unsafe unpersist :: (t <: Object) => Java (Dataset t) (Dataset t)

foreign import java unsafe unpersist2 :: (t <: Object) => Bool -> Java (Dataset t) (Dataset t)

foreign import java unsafe "where" where_ :: (t <: Object) => Column -> Java (Dataset t) (Dataset t)

foreign import java unsafe where2 :: (t <: Object) => String -> Java (Dataset t) (Dataset t)

foreign import java unsafe withColumn :: (t <: Object) => String -> Column -> Java (Dataset t) (Dataset row)

foreign import java unsafe withColumnRenamed :: (t <: Object) => String -> String -> Java (Dataset t) (Dataset row)

foreign import java unsafe withWatermark :: (t <: Object) => String -> String -> Java (Dataset t) (Dataset t)

foreign import java unsafe write :: (t <: Object) => Java (Dataset t) (DataFrameWriter t)

foreign import java unsafe writeStream :: (t <: Object) => Java (Dataset t) (DataFrameWriter t)
