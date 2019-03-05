import Data.List as List
modfiList::[Int]->Int->Int->[Int]
modfiList list index elm =  pre ++ [elm] ++ behind  where
            pre = List.take index list
            behind = List.drop (index+1) $ list;
main::IO()

closureFn::Int->Int->Int
closureFn param = (\x -> x+param+inner) where
    inner = 3

globalData = "a"
fn n = a where
    c = globalData
    t = putStrLn $ show globalData;
    a = t
main = fn 4