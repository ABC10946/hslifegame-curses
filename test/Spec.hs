import Lifegame

main :: IO ()
main = do
    let field = (foldl (fieldChange True) (initField (10,10))) [(2,2),(3,3),(3,4),(2,4),(1,4)]
    loop field 5

loop :: Field -> Int -> IO ()
loop field times = do
    if times /= 0 then do
        printField field
        loop (step field) (times-1)
    else
        return ()