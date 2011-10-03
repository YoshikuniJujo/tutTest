module Main where

import KeyPos
import System.Environment
import Paths_tutTest
import Codec.Binary.UTF8.String

main :: IO ()
main = do
	[str]	<- fmap (map decodeString) getArgs
	tutRule	<- getDataFileName "tutcode-rule.txt" >>= readFile >>= return . read
	keyPos	<- getDataFileName "keyboard-pos.txt" >>= readFile >>= return . read
	putStrLn $ unwords $ map showKeyPos $ strToTutPos keyPos tutRule str
