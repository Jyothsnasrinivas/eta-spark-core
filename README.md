# Spark Bindings for Eta

`eta-spark` is a library that provides complete bindings for Apache Spark.

## Example

The following shows the `SimpleApp` example from the Spark tutorial written using the Eta API.

```haskell
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}

import Java
import Spark.Core as S

checkLetter :: Char -> (forall a. JString -> Java a JBoolean)
checkLetter c str = return $ toJava ('a' `elem` str')
  where str' = fromJava str :: String

main :: IO ()
main = java $ do
  conf <- newSparkConf
  conf <.> setAppName "Simple Application"
  sc <- newSparkContext conf
  logData <- sc <.> textFile logfile >- cache
  numAs <- logData <.> S.filter (checkLetter 'a') >- count
  numBs <- logData <.> S.filter (checkLetter 'b') >- count
  io $ putStrLn $ "Lines with a: "   ++ show numAs
               ++ ", lines with b: " ++ show numBs
  sc <.> stop
  where logfile = "README.md"
```

This is the original Scala code.

```scala
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

object SimpleApp {
  def main(args: Array[String]) {
    val logFile = "YOUR_SPARK_HOME/README.md" // Should be some file on your system
    val conf = new SparkConf().setAppName("Simple Application")
    val sc = new SparkContext(conf)
    val logData = sc.textFile(logFile, 2).cache()
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println(s"Lines with a: $numAs, Lines with b: $numBs")
    sc.stop()
  }
}
```
## Installing

First clone this repository

`epm install https://github.com/Jyothsnasrinivas/eta-spark`

Go to the directory `cd eta-spark` and then `epm install`

## Running

Examples can be found in examples directory. To run an example, compile it with the following command:

```eta SimpleApp.hs```

This should generate a JAR file called `RunSimpleApp.jar`.

To run, go to the directory of your Spark installation and run the following command:

```
./bin/spark-submit --class eta.main --master local $PATH_TO_SIMPLEAPP/RunSimpleApp.jar
```

where `PATH_TO_SIMPLEAPP` is the path to `RunSimpleApp.jar`.
