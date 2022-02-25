module Main where
import PokerHand

litEntree:: IO [String]
litEntree = do
    ligne <- getLine
    if ligne == ""
        then return []
        else do
            suivantes <- litEntree
            return $ ligne:suivantes

main :: IO ()
main = do
    entree <- litEntree
    print entree
