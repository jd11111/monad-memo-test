module Main where

import qualified Control.Monad.State.Strict as CMSS
import qualified Data.HashMap.Strict as DHS
import qualified Data.Hashable
  
fib :: Int -> MyMemo Int Integer
fib 0 = return 0
fib 1 = return 1
fib n = myMemo fib (n-2) >>= (\ x -> myMemo fib (n-1) >>= (\ y -> return (x+y)))

type MyMemo a b = CMSS.State (DHS.HashMap a b) b

myMemo :: Data.Hashable.Hashable a => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = CMSS.gets (DHS.lookup x) >>= maybe z return where z = f x >>= (\ y -> CMSS.modify (DHS.insert x y) >> return y)

runMyMemo :: (a -> MyMemo a b) -> a -> b
runMyMemo f x = CMSS.evalState (f x) DHS.empty

main :: IO ()
main =do{print $ runMyMemo fib 3000;}