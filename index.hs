{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Bits as Bits
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the dynamicArray function below.
--
computeIndex::Int->Int->Int->Int
computeIndex query lasta n = rem (Bits.xor query lasta) n

len::[Int]->Int
len [] = 0
len (x:xs) = 1+(len xs)

lastAnswer::Int
lastAnswer = 0

-- modfiList::[Int]->Int->Int->[Int]
-- modfiList list index elm =  pre ++ [elm] ++ behind  where
--             pre = List.take index list
--             behind = List.drop (index+1) $ list;

modifArray::[[Int]]->Int->[Int]->[[Int]]
modifArray dict index el = a++[el]++b
    where 
        a = Data.List.take index dict
        b = Data.List.drop (index+1) dict

my_last::[Int]->Int
my_last list | list==[] = 0
        | otherwise = Data.List.last list
runQuery::Int->[[Int]] -> [Int] -> [[Int]]
runQuery n dict cmd= x where 
                    y = cmd !! 0
                    lastAnswer = my_last (dict !! n)
                    index = computeIndex (cmd!!1) lastAnswer n;
                    -- lastAnswer =  if (cmd !! 0) == 2 
                    --     then (dict !! index) !! (cmd !! 2)
                    --     else  my_last (dict !! n)
                    x = if (cmd !! 0) == 1 
                            then  modifArray dict index $ (dict !! index) ++ [(cmd !! 2)]
                            else modifArray dict n $  (dict !! n) ++ [ (dict !! index) !! (rem (cmd !! 2) $ len (dict !! index) )]

dynamicArray n queries = do
    -- let x = modifArray [[1],[2]] 0 [3] 
    let f = runQuery n
    let dict = Data.List.replicate (n+1) []
    let res = Data.List.foldl' f dict queries
    -- putStr  $ show $ (res)
    return (res !! n)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

my_fn::[Char] -> Int -> [Char]
my_fn "" x = (show x)
my_fn total x =  total ++ "\n" ++ (show x)

main :: IO()
main = do
    -- stdout <- getEnv "OUTPUT_PATH"
    let stdout = "/dev/stdout"
    fptr <- openFile stdout WriteMode

    nqTemp <- getLine
    let nq = Data.List.words $ rstrip nqTemp

    let n = read (nq !! 0) :: Int

    let q = read (nq !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ x) queriesTemp
    result <- dynamicArray n queries

    --hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map (\x -> show x) $ result
    hPutStrLn fptr $ Data.List.foldl' my_fn "" result
    hFlush fptr
    hClose fptr
