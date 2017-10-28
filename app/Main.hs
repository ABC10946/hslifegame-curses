module Main where

import Lifegame
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesHelper
import Control.Monad (forever)
import System.Exit (exitWith,ExitCode (..))

main :: IO ()
main = do
    -- loop field 100
    CursesHelper.start
    (height,width) <- Curses.scrSize
    let fieldInit = Lifegame.initField (width,height)
    let field = (foldl (fieldChange True) fieldInit) [(5,5),(6,6),(6,7),(5,7),(4,7)]
    loop field

quitTask :: IO ()
quitTask = do
    CursesHelper.end
    exitWith ExitSuccess

cursesOutput :: Field -> Int -> IO ()
cursesOutput field lineCount = do
    if lineCount /= 0 then do
        Curses.wAddStr Curses.stdScr (lineToString (field !! ((length field) - lineCount)))
        Curses.addLn
        cursesOutput field (lineCount - 1)
    else
        return ()

loop :: Field -> IO ()
loop field = do
    let sentineledField = addSentineled $ addSentineled field 
    cursesOutput sentineledField (length sentineledField)
    Curses.refresh
    CursesHelper.gotoTop
    Curses.timeout 500
    c <- CursesHelper.getKey (return ())
    case CursesHelper.displayKey c of "q" -> quitTask
                                      _   -> loop (step field)
