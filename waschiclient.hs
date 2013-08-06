module Main (main) where

import Network.HTTP
import System.Random
import Data.Array.IO
import Control.Monad

-- the possible actions this client can perform
data Action = Search String | Wash String
                        deriving(Eq,Show)

main :: IO ()
main = do
        putStrLn "Welcome to waschiclient-hs!\nA Waschi client written in Haskell!"
        -- get the waschi server-list and shuffle it
        servers <- getServers >>= shuffle
        act <- askUserAction
        putStrLn ("User wants to " ++ show act)

-- Action to ask the user which operation to perform
askUserAction :: IO Action
askUserAction = do
        putStrLn "What should I do?"
        putStrLn "1. wash something"
        putStrLn "2. search something"
        a <- getLine
        putStrLn "Enter the name of the clothing:"
        c <- getLine
        return (charToAction (head a,c))

-- parse the given char and translate it into an Action with the given String data
charToAction :: (Char,String) -> Action
charToAction ('1',s) = Wash s
charToAction ('2',s) = Search s
charToAction _ = error "Unknown Action"

-- get the serverlist and split it, then drop the last 11 chars (receive.php)
getServers :: IO [String]
getServers = fmap (map (reverse . drop 11 . reverse)) (simpleHTTP (getRequest "http://waschi.org/servers.php") >>= fmap words . getResponseBody)


-- shuffle from http://www.haskell.org/haskellwiki/Random_shuffle
--  Imperative because of better performance!
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
        where
                n = length xs
                newArray :: Int -> [a] -> IO (IOArray Int a)
                newArray n =  newListArray (1,n)

