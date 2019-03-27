{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import Data.Split
import Data.Bits as Bits
import Data.Set
import System.Environment
import System.IO

--
-- Complete the dynamicArray function below.
--
computeIndex::Int->Int->Int->Int
computeIndex query lasta n = rem (Bits.xor lasta query) n


lastAnswer::Int
lastAnswer = 0

modifArray::[[Int]]->Int->[Int]->[[Int]]
modifArray dict index el = a++[el]++b --todo 修改
    where 
        (a, b) = Data.List.splitAt (index-1) dict
        

runQuery::Int->[[Int]] -> [Int] -> [[Int]]

runQuery n dict cmd= x where 
                    y = cmd !! 0
                    index = computeIndex (cmd!!1) lastAnswer  n;
                    x = if (cmd !! 0) == 1 
                            then  modifArray dict index $ (dict !! index) ++ [(cmd !! 2)]
                            else  modifArray dict n $ (dict !! n) ++ [(dict !! index) !! 0]

dynamicArray n queries = do
    -- let x = modifArray [[1],[2]] 0 [3] 
    let f = runQuery n
    let res = Data.List.foldl f  (replicate (n+1) []) queries
    -- putStr  $ show $ (res)
    return (res !! n)

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
