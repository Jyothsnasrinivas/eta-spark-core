
module Spark.Core.Internal.SparkContext where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe "@new" newSparkContext :: SparkConf -> Java a SparkContext

-- foreign import java unsafe cache :: (Extends t Object) => RDD t (JavaRDD t)

foreign import java unsafe addFile :: String -> Java SparkContext ()

foreign import java unsafe addFile2 :: String -> Bool -> Java SparkContext ()

foreign import java unsafe addJar :: String -> Java SparkContext ()

foreign import java unsafe appName :: Java SparkContext String

foreign import java unsafe binaryFiles :: String -> Int -> Java SparkContext (JavaPairRDD JString PortableDataStream)

foreign import java unsafe cancelAllJobs :: Java SparkContext ()

foreign import java unsafe cancelJob :: Int -> Java SparkContext ()

foreign import java unsafe cancelJobGroup :: String -> Java SparkContext ()

foreign import java unsafe cancelStage :: Int -> Java SparkContext ()

foreign import java unsafe clearCallSite :: Java SparkContext ()

foreign import java unsafe clearJobGroup :: Java SparkContext ()

foreign import java unsafe close :: Java SparkContext ()

foreign import java unsafe collectionAccumulator :: (t <: Object)
                                                 => Java SparkContext (CollectionAccumulator t)

foreign import java unsafe collectionAccumulator2 :: (t <: Object)
                                                  => String
                                                  -> Java SparkContext (CollectionAccumulator t)

foreign import java unsafe defaultMinPartitions :: Java SparkContext Int

foreign import java unsafe defaultParallelism :: Java SparkContext Int

foreign import java unsafe deployMode :: Java SparkContext String

foreign import java unsafe doubleAccumulator :: Java SparkContext DoubleAccumulator

foreign import java unsafe doubleAccumulator2 :: String -> Java SparkContext DoubleAccumulator

foreign import java unsafe emptyRDD :: Java SparkContext (JavaRDD t)

-- foreign import java unsafe files :: Java SparkContext (Seq JString)

foreign import java unsafe getCheckpointDir :: Java SparkContext (Option JString)

foreign import java unsafe getConf :: Java SparkContext SparkConf

foreign import java unsafe getLocalProperty :: String -> Java SparkContext String

foreign import java unsafe "@static org.apache.spark.SparkContext" getOrCreate :: SparkContext

foreign import java unsafe "@static org.apache.spark.SparkContext" getOrCreate2 :: SparkConf -> SparkContext

foreign import java unsafe getSchedulingMode :: Java SparkContext Value

foreign import java unsafe hadoopConfiguration :: Java SparkContext Configuration

foreign import java unsafe isLocal :: Java SparkContext Bool

foreign import java unsafe isStopped :: Java SparkContext Bool

foreign import java unsafe longAccumulator :: Java SparkContext LongAccumulator

foreign import java unsafe "longAccumulator" longAccumulator2 :: String -> Java SparkContext LongAccumulator

foreign import java unsafe master :: Java SparkContext String

foreign import java unsafe range :: Int64 -> Int64 -> Int64 -> Int -> Java SparkContext (RDD Object)

foreign import java unsafe setCallSite :: String -> Java SparkContext ()

foreign import java unsafe setCheckpointDir :: String -> Java SparkContext ()

foreign import java unsafe setJobDescription :: String -> Java SparkContext ()

foreign import java unsafe setJobGroup :: String -> String -> Bool -> Java SparkContext ()

foreign import java unsafe setLocalProperty :: String -> String -> Java SparkContext ()

foreign import java unsafe setLogLevel :: String -> Java SparkContext ()

foreign import java unsafe sparkUser :: Java SparkContext String

foreign import java unsafe sparkTime :: Java SparkContext Int64

foreign import java unsafe statusTracker :: Java SparkContext SparkStatusTracker

foreign import java unsafe textFile :: String  -> Java SparkContext (JavaRDD JString)

foreign import java unsafe uiWebUrl :: Java SparkContext (Option JString)

foreign import java unsafe version :: Java SparkContext String

foreign import java unsafe stop :: Java SparkContext ()
