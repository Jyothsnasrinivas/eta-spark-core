{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}

import Java
import Spark.Core as S

checkLetter :: Char -> (forall a. JString -> Java a JBoolean)
checkLetter c str = return $ toJava (c `elem` str')
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
