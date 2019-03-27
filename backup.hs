{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (length)
import Data.Sequence hiding (replicate)
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Bits

readInts :: IO [Int]
readInts = map read <$> words <$> getLine

action1 :: (Int, Int) -> Int -> Seq (Seq Int) -> Seq (Seq Int)
action1 (x, y) lastans arr = adjust (\s -> s |> y) indice arr
    where
      indice = (x `xor` lastans) `mod` n
      n = length arr

action2 :: (Int, Int) -> Int -> Seq (Seq Int) -> IO Int
action2 (x, y) lastans arr = print elt >> return elt
    where
      elt = index s $ y `mod` (length s)
      s = index arr indice
      indice = (x `xor` lastans) `mod` n
      n = length arr

solve :: Int -> Int -> Seq (Seq Int) -> IO ()
solve 0 _ _ = return ()
solve q lastans arr = do
    [action, x, y] <- readInts -- Read one query
    (ans, newArr) <- case action of
                        1 -> return (lastans, action1 (x, y) lastans arr)
                        2 -> action2 (x, y) lastans arr >>= (\ans -> return (ans, arr))
                        _ -> undefined
    solve (q - 1) ans newArr
    
main :: IO ()
main = do
    [n, q] <- readInts
    let arr = fromList (replicate n (empty :: Seq Int))
    solve q 0 arr