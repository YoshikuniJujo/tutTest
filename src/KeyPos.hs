module KeyPos (

  KeyPos( PosEsc )
, getPos
, showTut
, readTutFile
, readTutFileLine
, readTutFileLineSep

, readTutFileLineWithDic
, readTutLineWithDic

, strToTutPos
, strToTutDic
, showTutDic

, showKeyPos

) where

import Data.Maybe   ( fromMaybe )
import Data.List    ( isPrefixOf, nub, (\\) )
import Graphics.Vty ( Vty, mkVty, standardIOConfig,
			update, nextEvent, shutdown, picForImage,
			string, Event( EvKey ), (<->) )
import Graphics.Vty.Attributes (currentAttr)
import Graphics.Vty.Input (Key( KChar, KEsc, KEnter))
import Control.Monad.Tools ( doWhile )
import Control.Arrow       ( first, second )
import System.IO           ( withFile, IOMode( ReadMode ), hGetLine )

main :: IO ()
main = do
  putStrLn "module KeyPos"
  posTable <- readFile "src/keyboard-pos.txt" >>= return . read
  tutTable <- readFile "src/tutcode-rule.txt" >>= return . read
  readTutFile posTable tutTable "test.txt" >>= print
  readTutFile posTable tutTable "test.txt" >>= putStrLn . showTut posTable tutTable
  cfg <- standardIOConfig
  vty <- mkVty cfg
  _ <- doWhile [ ] $ \pns -> do
    pos <- getPos posTable vty
    update vty $ picForImage $ ( string currentAttr "module KeyPos" <-> )
                               $ string currentAttr
                               $ showTut posTable tutTable ( pns ++ [ pos ] )
    return ( pns ++ [ pos ], pos /= PosEsc )
  shutdown vty
  putStrLn $ concatMap showTutDic $ strToTutDic posTable tutTable "本日は晴天なりなり"
  ( pos, dic ) <- readTutFileLineWithDic posTable tutTable "test.txt"
  putStrLn $ concatMap showKeyPos pos
  putStrLn $ concatMap showTutDic dic

data KeyPos = L Int Int | R Int Int | PosSpace | PosEsc | PosEnter | PosOut
              deriving ( Show, Eq, Read )

showKeyPos :: KeyPos -> String
showKeyPos ( L r c ) = "L" ++ show r ++ show c
showKeyPos ( R r c ) = "R" ++ show r ++ show c
showKeyPos PosSpace  = " 4 "
showKeyPos PosEsc    = "ESC"
showKeyPos PosEnter  = "ENT"
showKeyPos PosOut    = "---"

showTut :: [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> [ KeyPos ] -> String
showTut _     _      [ ]              = ""
showTut kbTbl tutTbl kps@( kp : kps' )
  = case makeTutChar ( convertTable kbTbl tutTbl ) kps of
         Just ( c, rest ) -> c : showTut kbTbl tutTbl rest
         Nothing          -> showKeyPos kp ++ showTut kbTbl tutTbl kps'

convertTable ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> [ ( [ KeyPos ], Char ) ]
convertTable kpTbl tutTbl
  = map ( first ( map $ charToKeyPos kpTbl ) . second head ) tutTbl

makeTutChar :: [ ( [ KeyPos ], Char ) ] -> [ KeyPos ] -> Maybe ( Char, [ KeyPos ] )
makeTutChar tbl pns
  = case filter ( (`isPrefixOf` pns) . fst ) tbl of
         ( kps, c ) : _ -> Just ( c , drop ( length kps ) pns )
         _              -> Nothing

charToKeyPos :: [ ( Char, KeyPos ) ] -> Char -> KeyPos
charToKeyPos tbl = fromMaybe PosOut . flip lookup tbl

getPosFromEvent :: [ ( Char, KeyPos ) ] -> Event -> Maybe KeyPos
getPosFromEvent tbl ( EvKey ( KChar c ) [ ] ) = Just $ charToKeyPos tbl c
getPosFromEvent _   ( EvKey KEsc         [ ] ) = Just PosEsc
getPosFromEvent _   ( EvKey KEnter       [ ] ) = Just PosEnter
getPosFromEvent _   ( EvKey _            _   ) = Just PosOut
getPosFromEvent _   _                          = Nothing

getPos :: [ ( Char, KeyPos ) ] -> Vty -> IO KeyPos
getPos tbl vty = doWhile PosOut $ \_ -> do
         ev <- nextEvent vty
         case getPosFromEvent tbl ev of
              Just pos -> return ( pos   , False )
              Nothing  -> return ( PosOut, True  )

strToTutPos :: [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> String -> [ KeyPos ]
strToTutPos kbTbl posTbl = concatMap $ tutCharToKeyPos $ convertTable kbTbl posTbl

strToTutDic :: [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> String -> [ ( Char, [ KeyPos ] ) ]
strToTutDic kbTbl posTbl str
  = map ( \( x, y ) -> ( y, x ) ) $ filter ( flip elem onlyKanji . snd )  $ convertTable kbTbl posTbl
  where onlyKanji = nub str \\ ( [ 'ぁ' .. 'ん' ] ++ "、。" )

showTutDic :: ( Char, [ KeyPos ] ) -> String
showTutDic ( ch, pos ) = ch : " " ++ concatMap showKeyPos pos ++ " "

readTutFile, readTutFileLine ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> String -> IO [ KeyPos ]
readTutFile kbTbl posTbl
  = fmap ( concatMap $ tutCharToKeyPos $ convertTable kbTbl posTbl ) . readFile

readTutFileLine kbTbl posTbl fn
  = withFile fn ReadMode $
      fmap ( concatMap $ tutCharToKeyPos $ convertTable kbTbl posTbl ) . hGetLine

readTutFileLineSep ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> String -> IO [ [ KeyPos ] ]
readTutFileLineSep kbTbl posTbl fn
  = withFile fn ReadMode $
      fmap ( map $ tutCharToKeyPos $ convertTable kbTbl posTbl ) . hGetLine

readTutLineWithDic :: [( Char, KeyPos)] -> [(String, [Char])] -> String ->
	([KeyPos], [(Char, [KeyPos])])
readTutLineWithDic kbTbl posTbl str =
	(strToTutPos kbTbl posTbl str, strToTutDic kbTbl posTbl str)

readTutFileLineWithDic ::
  [ ( Char, KeyPos ) ] -> [ ( String, [ Char ] ) ] -> String
                       -> IO ( [ KeyPos ], [ ( Char, [ KeyPos ] ) ] )
readTutFileLineWithDic kbTbl posTbl fn = do
  ln <- withFile fn ReadMode hGetLine
  return ( strToTutPos kbTbl posTbl ln, strToTutDic kbTbl posTbl ln )

tutCharToKeyPos :: [ ( [ KeyPos ], Char ) ] -> Char -> [ KeyPos ]
tutCharToKeyPos _   '\n' = [ PosEnter ]
tutCharToKeyPos tbl ch
  = fromMaybe [ PosSpace ] $ lookup ch $ map ( \( ps, c ) -> ( c, ps ) ) tbl
