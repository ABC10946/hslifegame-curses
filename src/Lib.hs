module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    let field = fieldChange (2,2) (initField (10,10)) True
    printField field

type ScreenSize = (Int,Int)
type Field = [[Bool]]
type Line = [Bool]
type Position = (Int,Int)
type LinePosition = Int



initField :: ScreenSize -> Field
initField (width,height) = take height $ repeat $ take width $ repeat False

fieldChange :: Position -> Field -> Bool -> Field
fieldChange (x,y) (line:lines) newValue
    |y == 0 = newLine:lines
    |otherwise = line:(fieldChange (x,y-1) lines newValue)
    where
        newLine = lineChange x line newValue
        

lineChange :: LinePosition -> Line -> Bool -> Line
lineChange linePos (x:xs) newValue
    |linePos == 0 = newValue:xs
    |otherwise = x:(lineChange (linePos-1) xs newValue)

printField :: Field -> IO ()
printField field = do
    let field_ = fieldToString field
    putStrLn field_

lineToString :: Line -> String
lineToString line = jointStringList [if x then "#" else "_"|x <- line]

fieldToString :: Field -> String
fieldToString field = jointStringList $ map (++"\n") $ map lineToString field

jointStringList :: [String] -> String
jointStringList text = (foldl (++) "") text