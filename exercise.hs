import Data.Maybe as Maybe
import Data.List as List
modfiList::[Int]->Int->Int->[Int]
modfiList list index elm =  pre ++ [elm] ++ behind  where
            pre = List.take index list
            behind = List.drop (index+1) $ list;
main::IO()

closureFn::Int->Int
closureFn  = (\x -> x+1+inner) where
    inner = 3

globalData = "a"
fn n = a where
    c = globalData
    t = putStrLn $ show globalData;
    a = t

-- testFn::Maybe Int -> Maybe Int
realFn::Maybe a -> Either (Maybe a) (Maybe a -> Either  (Maybe a) b)
realFn (Just a ) = Right (realFn)
realFn Nothing = Left $ Just 2
-- wrapFn :: Int-> Maybe Int -> realFn
main = putStrLn $ show $ closureFn 2