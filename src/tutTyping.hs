module Main where

import Paths_tutTest
import System.Environment
import KeyPos
import Graphics.Vty
import Control.Monad.Tools
import Data.Time
import Numeric

main :: IO ()
main = do
  [ fn ]       <- getArgs
  tutRule      <- getDataFileName "tutcode-rule.txt" >>= readFile >>= return . read
  keyPos       <- getDataFileName "keyboard-pos.txt" >>= readFile >>= return . read
  cnt          <- readFile fn
  cfg <- standardIOConfig
  vty <- mkVty cfg
  (width, height) <- displayBounds =<< outputForConfig cfg
  (g, w, t) <-  foreach (lines cnt) $ \ln -> do
    let ( pos, dic ) = readTutLineWithDic keyPos tutRule ln
    5 `timesDo` ( 0, 1, 1 ) $ \n ( g0, w0, t0 ) -> do
      ( g, w, t ) <- doWhile ( g0, w0, t0 ) $ \( good, whole, time ) -> do
        ( g, w, t, fin ) <- runTutTyping keyPos tutRule vty pos dic n ( good, whole, time )
        return $ ( if fin then ( g, w, t ) else ( good, whole, time ),
                   fromIntegral g / ( fromIntegral w :: Double ) < 0.95 ||
                   fromIntegral w / t < 3 )
      return ( g, w, t )
  shutdown vty
  putStrLn $ showFFloat ( Just 0 )
                        ( fromIntegral g / fromIntegral w * 100 :: Double ) "" ++
             "% "  ++
             show ( fromIntegral ( floor ( fromIntegral w / t * 100 ) :: Int ) /
                    100 :: Double )

runTutTyping ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> Vty
                       -> [ KeyPos ] -> [ ( Char, [ KeyPos ] ) ] -> Int -> ( Int, Int, NominalDiffTime )
                       -> IO ( Int, Int, NominalDiffTime, Bool )
runTutTyping keyPos tutRule vty str dic n ( g, w, t ) = do
  cfg <- standardIOConfig
  (wt, ht) <- displayBounds =<< outputForConfig cfg
  let width = fromIntegral wt
      img = string currentAttr $ show n ++ " " ++ showTut keyPos tutRule str
      ret = show g ++ "/" ++ show w ++ " " ++
            showFFloat ( Just 0 )
                       ( fromIntegral g / ( fromIntegral w :: Double ) * 100 ) ""
            ++  "% "
            ++ showSpeed t ++ "sec " ++
            showSpeed ( fromIntegral w / t ) ++ "key/sec"
  update vty $ picForImage $ ( img <-> )
                             $ ( flip ( foldl (<->) )
                                 $ map ( string currentAttr )
                                   ( splitString
                                     ( width `div` 2 ) "" $ map showTutDic dic ) )
                             $ ( !! 8 ) $ iterate ( <-> string currentAttr " " )
                             $ ( <-> string currentAttr ret )
                             $ ( !! 8 ) $ iterate ( <-> string currentAttr " " )
                             $ string currentAttr " "
  pre <- getCurrentTime
  ( input, fin ) <- doWhile ( [ ], True ) $ \( pns, _ ) -> do
    p <- getPos keyPos vty
    update vty $ picForImage $ ( img <-> )
                               $ ( flip ( foldl (<->) ) $ map ( string currentAttr )
                                                              ( splitString
                                                                 ( width `div` 2 ) "" $ map showTutDic dic ) )
                               $ ( !! 8 ) $ iterate ( <-> string currentAttr " " )
                               $ ( <-> string currentAttr ret )
                               $ ( !! 8 ) $ iterate ( <-> string currentAttr " " )
                               $ string currentAttr
                               $ ( replicate ( 1 + length ( show n ) ) ' ' ++ )
                               $ showTut keyPos tutRule
                               $  pns ++ [ p ]
    return ( ( pns ++ [ p ], p /= PosEsc ),
             p /= PosEsc && length ( pns ++ [ p ] ) < length str )
  post <- getCurrentTime
  return ( length $ filter id $ zipWith (==) input str, length str,
           diffUTCTime post pre, fin )

timesDo :: Monad m => Int -> a -> ( Int -> a -> m a ) -> m a
( 0 `timesDo` x ) _   = return x
( n `timesDo` x ) act = do
  x' <- act n x
  ( n - 1 ) `timesDo` x' $ act

showSpeed :: NominalDiffTime -> String
showSpeed s = show $ fromIntegral ( floor $ s * 100 :: Int ) / ( 100 :: Double )

splitString :: Int -> String -> [ String ] -> [ String ]
splitString _ "" [ ]        = [ ]
splitString _ r  [ ]        = [ r ]
splitString n r  sa@( s : ss )
  | length ( r ++ s ) > n    = r : splitString n "" sa
  | otherwise                = splitString n ( r ++ s ) ss

foreach :: [a] -> (a -> IO b) -> IO b
foreach [x] f		= f x
foreach (x : xs) f	= f x >> foreach xs f
