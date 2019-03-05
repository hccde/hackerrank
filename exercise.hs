import Data.List as List
modfiList::[Int]->Int->Int->[Int]
modfiList list index elm =  pre ++ [elm] ++ behind  where
            pre = List.take index list
            behind = List.drop (index+1) $ list;
main::IO()

closureFn::Int->Int->Int
closureFn param = (\x -> x+param+inner) where
    inner = 3

main = putStrLn $ show 1