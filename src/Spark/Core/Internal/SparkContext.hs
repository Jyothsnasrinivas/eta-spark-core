{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.SparkContext where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe "@new" newSparkContext :: SparkConf -> Java a SparkContext

foreign import java unsafe "textFile" textFile :: JString -> Int -> Java SparkContext (RDD JString)

foreign import java unsafe stop :: Java SparkContext ()

foreign import java unsafe cache :: (Extends t Object) => RDD t (JavaRDD t)

foreign import java unsafe addFile :: JString -> Java SparkContext ()

foreign import java unsafe addFile2 :: JString -> Bool -> Java SparkContext ()

foreign import java unsafe addJar :: JString -> Java SparkContext ()

foreign import java unsafe applicationAttemptId :: Java SparkContext (Option JString)

foreign import java unsafe applicationId :: Java SparkContext JString

foreign import java unsafe appName :: Java SparkContext JString

foreign import java unsafe binaryFiles :: JString -> Int -> Java SparkContext (RDD (Tuple2 JString PortableDataStream))

foreign import java unsafe cancelAllJobs :: Java SparkContext ()

foreign import java unsafe cancelJob :: Int -> Java SparkContext ()

foreign import java unsafe cancelJobGroup :: JString -> Java SparkContext ()

foreign import java unsafe cancelStage :: Int -> Java SparkContext ()

foreign import java unsafe clearCallSite :: Java SparkContext ()

foreign import java unsafe clearJobGroup :: Java SparkContext ()

foreign import java unsafe collectionAccumulator :: (t <: Object)
                                                 => Java SparkContext (CollectionAccumulator t)

foreign import java unsafe collectionAccumulator2 :: (t <: Object)
                                                  => Jstring
                                                  -> Java SparkContext (CollectionAccumulator t)

foreign import java unsafe defaultMinPartitions :: Java SparkContext Int

foreign import java unsafe defaultParallelism :: Java SparkContext Int

foreign import java unsafe deployMode :: Java SparkContext JString

foreign import java unsafe doubleAccumulator :: Java SparkContext DoubleAccumulator

foreign import java unsafe doubleAccumulator2 :: JString -> Java SparkContext DoubleAccumulator

foreign import java unsafe files :: Java SparkContext (Seq JString)

foreign import java unsafe getCheckpointDir :: Java SparkContext (Option JString)

foreign import java unsafe getConf :: Java SparkContext SparkConf

foreign import java unsafe getLocalProperty :: JString -> Java SparkContext JString

foreign import java unsafe "@static org.apache.spark.SparkContext" getOrCreate :: SparkContext

foreign import java unsafe "@static org.apache.spark.SparkContext" getOrCreate2 :: SparkConf -> SparkContext

foreign import java unsafe getSchedulingMode :: Java SparkContext Value

foreign import java unsafe hadoopConfiguration :: Java SparkContext Configuration

foreign import java unsafe isLocal :: Java SparkContext Bool

foreign import java unsafe isStopped :: Java SparkContext Bool

foreign import java unsafe longAccumulator :: Java SparkContext LongAccumulator

foreign import java unsafe "longAccumulator" longAccumulator2 :: JString -> Java SparkContext LongAccumulator

foreign import java unsafe master :: Java SparkContext JString

foreign import java unsafe range :: Int64 -> Int64 -> Int64 -> Int -> Java SparkContext (RDD Object)

foreign import java unsafe setCallSite :: JString -> Java SparkContext ()

foreign import java unsafe setCheckpointDir :: JString -> Java SparkContext ()

foreign import java unsafe setJobDescription :: JString -> Java SparkContext ()

foreign import java unsafe setJobGroup :: JString -> JString -> Bool -> Java SparkContext ()

foreign import java unsafe setLocalProperty :: JString -> JString -> Java SparkContext ()

foreign import java unsafe setLogLevel :: JString -> Java SparkContext ()

foreign import java unsafe sparkUser :: Java SparkContext JString

foreign import java unsafe sparkTime :: Java SparkContext Int64

foreign import java unsafe statusTracker :: Java SparkContext SparkStatusTracker

foreign import java unsafe stop :: Java SparkContext ()

foreign import java unsafe uiWebUrl :: Java SparkContext (Option JString)

foreign import java unsafe version :: Java SparkContext JString
