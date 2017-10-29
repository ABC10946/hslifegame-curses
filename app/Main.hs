module Main where

import Lifegame
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesHelper
import System.Exit (exitWith,ExitCode (..))

data Mode = Play | Stop deriving Eq

main :: IO ()
main = do
    CursesHelper.start
    (height,width) <- Curses.scrSize
    let fieldInit = Lifegame.initField (width,height)
    let field = (foldl (fieldChange True) fieldInit) [(5,5),(6,6),(6,7),(5,7),(4,7)]
    loop field Play

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

loop :: Field -> Mode -> IO ()
loop field mode = do
    Curses.timeout 1000
    let sentineledField = addSentinel $ addSentinel field
    cursesOutput sentineledField (length sentineledField)
    Curses.refresh
    CursesHelper.gotoTop
    c <- Curses.getch
    case CursesHelper.displayKey (Curses.decodeKey c) of "q" -> quitTask
                                                         "f" -> loop field (changeMode mode)
                                                         _   -> nextLoop mode

    where
        nextLoop :: Mode -> IO ()
        nextLoop mode
            |mode == Play = loop (step field) mode
            |mode == Stop = loop field mode

changeMode :: Mode -> Mode
changeMode Play = Stop
changeMode Stop = Play