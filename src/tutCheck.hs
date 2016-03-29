module Main where

import KeyPos
import System.Environment
import Paths_tutTest

main :: IO ()
main = do
	[str]	<- getArgs
	tr	<- tutRule
	kp	<- keyPos
	putStrLn $ unwords $ map showKeyPos $ strToTutPos kp tr str

tutRule :: IO [(String, [Char])]
tutRule	= getDataFileName "tutcode-rule.txt" >>= readFile >>= return . read

keyPos :: IO [(Char, KeyPos)]
keyPos	= getDataFileName "keyboard-pos.txt" >>= readFile >>= return . read
