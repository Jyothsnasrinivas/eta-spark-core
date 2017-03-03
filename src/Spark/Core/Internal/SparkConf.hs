
module Spark.Core.Internal.SparkConf where

import Java
import Spark.Core.Internal.Types

foreign import java unsafe "@new" newSparkConf :: Java c SparkConf

foreign import java unsafe setAppName :: String -> Java SparkConf SparkConf

foreign import java unsafe set :: String -> String -> Java SparkConf SparkConf

foreign import java unsafe clone :: Java SparkConf SparkConf

foreign import java unsafe contains :: String -> Java SparkConf Bool

foreign import java unsafe get :: String -> Java SparkConf String

foreign import java unsafe "get" get2 :: String -> String -> Java SparkConf String

foreign import java unsafe getAll :: Java SparkConf (Tuple2Array JString JString)

foreign import java unsafe getAllWithPrefix :: String -> Java SparkConf [Tuple2 JString JString]

foreign import java unsafe getAppId :: Java SparkConf String

foreign import java unsafe getBoolean :: String -> Bool -> Java SparkConf Bool

foreign import java unsafe getDeprecatedConfig :: String -> SparkConf -> Java SparkConf (Option JString)

foreign import java unsafe getDouble :: String -> Double -> Java SparkConf Double

foreign import java unsafe getInt :: String -> Int -> Java SparkConf Int

foreign import java unsafe getLong :: String -> Int64 -> Java SparkConf Int64

foreign import java unsafe getOption :: String -> Java SparkConf (Option JString)

foreign import java unsafe getSizeAsBytes :: String -> Java SparkConf Int64

foreign import java unsafe "getSizeAsBytes" getSizeAsBytes2 :: String -> Int64 -> Java SparkConf Int64

foreign import java unsafe "getSizeAsBytes" getSizeAsBytes3 :: String -> String -> Java SparkConf Int64

foreign import java unsafe getSizeAsGb :: String -> Java SparkConf Int64

foreign import java unsafe "getSizeAsGb" getSizeAsGb2 :: String -> String -> Java SparkConf Int64

foreign import java unsafe getSizeAsKb :: String -> Java SparkConf Int64

foreign import java unsafe "getSizeAsKb" getSizeAsKb2 :: String -> String -> Java SparkConf Int64

foreign import java unsafe getSizeAsMb :: String -> Java SparkConf Int64

foreign import java unsafe "getSizeAsMb" getSizeAsMb2 :: String -> String -> Java SparkConf Int64

foreign import java unsafe getTimeAsMs :: String -> Java SparkConf Int64

foreign import java unsafe "getTimeAsMs" getTimeAsMs2 :: String -> String -> Java SparkConf Int64

foreign import java unsafe getTimeAsSeconds :: String -> Java SparkConf Int64

foreign import java unsafe "getTimeAsSeconds" getTimeAsSeconds2 :: String -> String -> Java SparkConf Int64

foreign import java unsafe remove :: String -> Java SparkConf SparkConf

-- foreign import java unsafe set :: String -> String -> Java SparkConf SparkConf

-- foreign import java unsafe setAppName :: String -> Java SparkConf SparkConf

foreign import java unsafe setExecutorEnv :: String -> String -> Java SparkConf SparkConf

foreign import java unsafe setIfMissing :: String -> String -> Java SparkConf SparkConf

foreign import java unsafe setJars :: JStringArray -> Java SparkConf SparkConf

foreign import java unsafe setMaster :: String -> Java SparkConf SparkConf

foreign import java unsafe setSparkHome :: String -> Java SparkConf SparkConf

foreign import java unsafe "toDebugString" toDebugStringConf :: Java SparkConf String
