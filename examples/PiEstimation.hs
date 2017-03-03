{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
-- val count = sc.parallelize(1 to NUM_SAMPLES).filter { _ =>
--   val x = math.random
--   val y = math.random
--   x*x + y*y < 1
-- }.count()
-- println(s"Pi is roughly ${4.0 * count / NUM_SAMPLES}")

import GHC.Prim
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
  io $ putStrLn $ "Lines with a:   " ++ show numAs
               ++ ", lines with b: " ++ show numBs
  sc <.> stop
  where logfile = "README.md"
