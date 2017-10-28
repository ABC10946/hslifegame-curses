import Lifegame
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesHelper
import Control.Monad (forever)
import System.Exit (exitWith,ExitCode (..))

main :: IO ()
main = do
    CursesHelper.start
    (height,width) <- Curses.scrSize
    let field = Lifegame.initField (width-5,height-5)
    cursesOutput field (length field)
    forever $ do
        c <- Curses.getCh -- getCh is not worked in 'stack test'
        case CursesHelper.displayKey c of "q" -> quitTask
                                          _   -> return ()

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

loop :: Field -> Int -> IO ()
loop field times = do
    if times /= 0 then do
        printField $ addSentineled $ addSentineled field
        loop (step field) (times-1)
    else
        return ()