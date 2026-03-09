import Data.List (intercalate)

main :: IO ()
main = do
  content <- readFile "test-onenote.log"
  let linesInFile = lines content
  let totalLines = length linesInFile
  
  putStrLn $ "Total lines in file: " ++ show totalLines
  putStrLn ""
  
  putStrLn "Lines 20-35:"
  mapM_ (\(i, line) -> 
    let lineNum = i + 1
        display = if null line 
                  then "<EMPTY>"
                  else take 70 line ++ (if length line > 70 then "..." else "")
    in putStrLn $ "Line " ++ show lineNum ++ ": " ++ display
    ) (zip [19..] (drop 19 (take 35 linesInFile)))
  
  putStrLn ""
  putStrLn "Summary of empty lines:"
  let emptyLineNums = [(i+1, line) | (i, line) <- zip [0..] linesInFile, null line]
  if null emptyLineNums
    then putStrLn "No empty lines found"
    else mapM_ (\(lineNum, _) -> putStrLn $ "  Line " ++ show lineNum) emptyLineNums
