import Lifegame

main :: IO ()
main = do
    let field = (foldl (fieldChange True) (initField (30,20))) [(5,5),(6,6),(6,7),(5,7),(4,7)]
    loop field 100

loop :: Field -> Int -> IO ()
loop field times = do
    if times /= 0 then do
        printField $ addSentineled $ addSentineled field
        loop (step field) (times-1)
    else
        return ()