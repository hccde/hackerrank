import Data.List as List
modfiList::[Int]->Int->Int->[Int]
modfiList list index elm =  pre ++ [elm] ++ behind  where
            pre = List.take index list
            behind = List.drop (index+1) $ list;
main::IO()

main = putStrLn $ show $ modfiList [1,2,3,4,5,6,7] 3 3