{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Time.Clock
import Data.Time.Format
import Data.Fixed
import System.IO

main :: IO ()
main = printActivationDate (deprecationHeight 2496122) 2545575

getBlockDelta :: IO ()
getBlockDelta = do
  now <- getCurrentTime
  putStrLn $ "Current time is: " <> show now
  putStr $ "Input target date in format %Y-%m-%dT%H:%M:%SZ: "
  hFlush stdout
  dateLine <- getLine
  putStr $ "Input current block height as an integer: "
  hFlush stdout
  heightLine <- getLine
  printBlockDelta dateLine heightLine

printBlockDelta :: String -> String -> IO ()
printBlockDelta target curHeight = do
  t <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" target
  now <- getCurrentTime
  putStr $ "Block delta is "
  let delta = truncate $ toRational (diffUTCTime t now) / 75
  putStrLn . show $ delta
  let current :: [(Integer, String)] = reads curHeight
  case current of
    [(cur, _)] -> putStrLn $ "Target height is " <> (show $ cur + delta)
    _ -> putStrLn ("Could not parse current height from string " <> curHeight)

activationDate :: Integer -> Integer -> IO UTCTime
activationDate targetHeight curHeight = do
  now <- getCurrentTime
  let diffTime = fromIntegral $ (targetHeight - curHeight) * 75
  pure $ addUTCTime diffTime now

printActivationDate :: Integer -> Integer -> IO ()
printActivationDate targetHeight curHeight = do
  activationDate targetHeight curHeight >>= (putStrLn . show)

deprecationHeight :: Integer -> Integer
deprecationHeight rh =
  let deprecationWeeks = 16
      postBlossomSpacing = 75
      expectedBlocksPerHour = 3600 `div` postBlossomSpacing
      activationToDeprecationBlocks = deprecationWeeks * 7 * 24 * expectedBlocksPerHour
   in rh + activationToDeprecationBlocks

printDeprecationHeight :: Integer -> IO ()
printDeprecationHeight rh = do
  putStrLn $ "Expected height: " <> show (deprecationHeight rh)


