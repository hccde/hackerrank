{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Bits as Bits
import Data.Set
import System.Environment
import System.IO

--
-- Complete the dynamicArray function below.
--
computeIndex::Int->Int->Int->Int
computeIndex query lasta n = rem (Bits.xor lasta query) n


runQuery::[Int]-> Int
lastAnswer::Int
lastAnswer = 0
dict =  replicate 1 []
runQuery cmd = x where 
                    y = cmd !! 0
                    lastAnswer = y
                    index = computeIndex (cmd!!1) $ lastAnswer;
                    -- dict!!index = (cmd!!3):(dict!!index)
                    x
                        | y == 1 = 1
                        | y == 2 = 2

dynamicArray n queries = do
    let res = Data.List.map (\x -> runQuery x) queries
    -- putStr  $ show $ computeIndex 1 lastAnswer n
    return res
    where 
        {putStr "ok"}
        -- dict = replicate n []
        -- putStr $ show dict
    -- Data.List.map 
    --
    -- Write your code here.
    --

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    -- stdout <- getEnv "OUTPUT_PATH"
    let stdout = "/dev/stdout"
    fptr <- openFile stdout WriteMode

    nqTemp <- getLine
    let nq = words nqTemp

    let n = read (nq !! 0) :: Int

    let q = read (nq !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) queriesTemp
    result <- dynamicArray n queries

    hPutStrLn fptr $ intercalate "\n" $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
