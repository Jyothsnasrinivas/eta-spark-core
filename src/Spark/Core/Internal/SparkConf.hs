{-# LANGUAGE MagicHash #-}
module Spark.Core.Internal.SparkConf where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe "@new" newSparkConf :: Java c SparkConf

foreign import java unsafe setAppName :: String -> Java SparkConf SparkConf

foreign import java unsafe set :: String -> String -> Java SparkConf SparkConf

foreign import java unsafe clone :: Java SparkConf SparkConf

foreign import java unsafe contains :: JString -> Java SparkConf Bool

foreign import java unsafe get :: JString -> Java SparkConf JString

foreign import java unsafe "get" get2 :: JString -> JString -> Java SparkConf JString

foreign import java unsafe getAll :: Java SparkConf (Tuple2Array JString JString)

foreign import java unsafe getAllWithPrefix :: JString -> Java SparkConf [Tuple2 JString JString]

foreign import java unsafe getAppId :: Java SparkConf JString

foreign import java unsafe getBoolean :: JString -> Bool -> Java SparkConf Bool

foreign import java unsafe getDeprecatedConfig :: JString -> SparkConf -> Java SparkConf (Option JString)

foreign import java unsafe getDouble :: JString -> Double -> Java SparkConf Double

foreign import java unsafe getInt :: JString -> Int -> Java SparkConf Int

foreign import java unsafe getLong :: JString -> Int64 -> Java SparkConf Int64

foreign import java unsafe getOption :: JString -> Java SparkConf (Option JString)

foreign import java unsafe getSizeAsBytes :: JString -> Java SparkConf Int64

foreign import java unsafe "getSizeAsBytes" getSizeAsBytes2 :: JString -> Int64 -> Java SparkConf Int64

foreign import java unsafe "getSizeAsBytes" getSizeAsBytes3 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe getSizeAsGb :: JString -> Java SparkConf Int64

foreign import java unsafe "getSizeAsGb" getSizeAsGb2 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe getSizeAsKb :: JString -> Java SparkConf Int64

foreign import java unsafe "getSizeAsKb" getSizeAsKb2 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe getSizeAsMb :: JString -> Java SparkConf Int64

foreign import java unsafe "getSizeAsMb" getSizeAsMb2 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe getTimeAsMs :: JString -> Java SparkConf Int64

foreign import java unsafe "getTimeAsMs" getTimeAsMs2 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe getTimeAsSeconds :: JString -> Java SparkConf Int64

foreign import java unsafe "getTimeAsSeconds" getTimeAsSeconds2 :: JString -> JString -> Java SparkConf Int64

foreign import java unsafe remove :: JString -> Java SparkConf SparkConf

-- foreign import java unsafe set :: JString -> JString -> Java SparkConf SparkConf

-- foreign import java unsafe setAppName :: JString -> Java SparkConf SparkConf

foreign import java unsafe setExecutorEnv :: JString -> JString -> Java SparkConf SparkConf

foreign import java unsafe setIfMissing :: JString -> JString -> Java SparkConf SparkConf

foreign import java unsafe setJars :: JStringArray -> Java SparkConf SparkConf

foreign import java unsafe setMaster :: JString -> Java SparkConf SparkConf

foreign import java unsafe setSparkHome :: JString -> Java SparkConf SparkConf

foreign import java unsafe toDebugString :: Java SparkConf JString
