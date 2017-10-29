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
    let field = Lifegame.initField ((width+3),(height+3))
    loop field Play (0,0)

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

loop :: Field -> Mode -> (Int,Int) -> IO ()
loop field mode (x,y) = do
    Curses.timeout 1000
    let sentineledField = addSentinel $ addSentinel field
    CursesHelper.gotoTop
    cursesOutput sentineledField (length sentineledField)
    Curses.move y x
    Curses.refresh
    c <- CursesHelper.displayKey <$> Curses.decodeKey <$> Curses.getch

    (height,width) <- Curses.scrSize

    case c of
        "q" -> quitTask
        "f" -> loop field (changeMode mode) (x,y)
        "h" -> loop field mode ((max (x-1) 0),y)
        "l" -> loop field mode ((min (x+1) (width-1)),y)
        "k" -> loop field mode (x,(max (y-1) 0))
        "j" -> loop field mode (x,(min (y+1) (height-1)))
        "o" -> loop (fieldFlip field (x+2,y+2)) mode (x,y)
        _   -> nextLoop mode

    where
        nextLoop :: Mode -> IO ()
        nextLoop mode
            |mode == Play = loop (step field) mode (x,y)
            |mode == Stop = loop field mode (x,y)

        changeMode :: Mode -> Mode
        changeMode Play = Stop
        changeMode Stop = Play