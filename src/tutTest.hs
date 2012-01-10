module Main where

import Paths_tutTest
import KeyPos
import System.Environment( getArgs )
import Graphics.Vty
import Control.Monad.Tools
import Control.Monad ( zipWithM )
import System.IO

main :: IO ()
main = do
  [ fn ] <- getArgs
  tutRule <- getDataFileName "tutcode-rule.txt" >>= readFile >>= return . read
  keyPos  <- getDataFileName "keyboard-pos.txt" >>= readFile >>= return . read
--  cnt  <- readTutFileLine keyPos tutRule fn
  cnt' <- readTutFileLineSep keyPos tutRule fn
  vty <- mkVty
  ret <- zipWithM ( runTutTest keyPos tutRule vty ) cnt' [ 1 .. ]
--  mapM_ ( runTutTest keyPos tutRule vty ) cnt'
  shutdown vty
  hPutStrLn stderr $ concatMap ( showTut keyPos tutRule ) ret

runTutTest ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> Vty -> [ KeyPos ]
                       -> Int -> IO [ KeyPos ]
runTutTest keyPos tutRule vty str n = do
  let img = string current_attr $ show n ++ " " ++ showTut keyPos tutRule str
  update vty $ pic_for_image img
  ret <- doWhile [ ] $ \pns -> do
    pos <- getPos keyPos vty
    update vty $ pic_for_image $ ( img <-> )
                               $ string current_attr
                               $ replicate ( length $ show n ) ' ' ++ " " ++
                                 showTut keyPos tutRule ( pns ++ [ pos ] )
    if pos == PosEsc
       then return ( str, False ) -- True )
       else if length ( pns ++ [ pos ] ) < length str
               then return ( pns ++ [ pos ], pos /= PosEsc         )
               else return ( [ ],            pns ++ [ pos ] /= str )
  return ret
