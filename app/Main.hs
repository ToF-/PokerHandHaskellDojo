module Main where
import PokerHand

main :: IO ()
main = do
    entree <- getContents
    let sortie = resultat (map words (lines entree))
    putStrLn $ unlines sortie

