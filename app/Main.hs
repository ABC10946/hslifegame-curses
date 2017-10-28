module Main where

import Lifegame
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesHelper
import System.Exit (exitWith,ExitCode (..))

main :: IO ()
main = do
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
    Curses.timeout 1000
    let sentineledField = addSentinel $ addSentinel field
    cursesOutput sentineledField (length sentineledField)
    Curses.refresh
    CursesHelper.gotoTop
    c <- Curses.getch
    case CursesHelper.displayKey (Curses.decodeKey c) of "q" -> quitTask
                                                         _   -> loop (step field)
