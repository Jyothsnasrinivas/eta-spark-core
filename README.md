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

0. Install [etlas](https://eta-lang.org/docs/user-guides/eta-user-guide/installation/etlas).

0. Clone this repository.

`git clone https://github.com/Jyothsnasrinivas/eta-spark-core`

0. Go to the newly-cloned directory.

`cd eta-spark-core`

0. Build it.

`etlas build eta-spark-core`
`etlas build simple-app-exe --enable-uberjar-mode`

0. Run it.

`./bin/spark-submit --class eta.main --master local dist/<path-to-simple-app-exe>/simple-app-exe.jar`
More examples can be found in examples directory.
