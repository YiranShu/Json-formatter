import System.IO


inQuotation :: Bool -> Bool -> String -> [Bool] -- isEscape -> inQuotation -> string -> result
inQuotation a b (x : []) = [b]
inQuotation a b (x: xs) | x == '"' && (not a) = b: (inQuotation a (not b) xs)
                        | x == '\\' = b: (inQuotation True b xs)
                        | x /= '"' && (not a) = b: (inQuotation a b xs)
                        | x == '"' && a = b: (inQuotation (not a) b xs)
                        | x /= '"' && a = b: (inQuotation False b xs)
                        | otherwise = b: (inQuotation False b xs)


startWith :: String -> String -> Bool
startWith start s = if length s >= length start && take (length start) s == start then True else False


isDigit :: String -> Bool
isDigit s | s == "+" = True
          | s == "-" = True
          | s == "." = True
          | s == "E" = True
          | s == "e" = True
          | elem s (map show [0..9]) = True
          | otherwise = False


cut :: String -> Int -> String -- take out the first few characters of the string
cut s 0 = s
cut "" number = ""
cut s number = cut (tail s) (number - 1)


push :: String -> [String] -> [String] --push string into stack
push s stack = s: stack


pop :: [String] -> [String]
pop stack = tail stack


top :: [String] -> String
top [] = "empty"
top stack = stack !! 0


getSubstring :: String -> Int -> Int -> String
getSubstring "" start end = ""
getSubstring str start end | start == end = [str !! start]
                           | otherwise = [str !! start] ++ getSubstring str (start + 1) end


numberOfDigit :: String -> Int
numberOfDigit "" = 0
numberOfDigit (x: xs) | not (isDigit [x]) = 0
                      | otherwise = (1 + numberOfDigit xs)


scanner :: Int -> String -> [Bool] -> String -> [String] -- index -> string -> inQuotation -> buffer -> result
scanner index "" isInQuotation buffer = []
scanner index s isInQuotation buffer | startWith "{" s && isInQuotation !! index == False = ["{"] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith "}" s && isInQuotation !! index == False = ["}"] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith "[" s && isInQuotation !! index == False = ["["] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith "]" s && isInQuotation !! index == False = ["]"] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith ":" s && isInQuotation !! index == False = [":"] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith "," s && isInQuotation !! index == False = [","] ++ scanner (index + 1) (cut s 1) isInQuotation buffer
                                     | startWith "true" s && isInQuotation !! index == False = ["true"] ++ scanner (index + 4) (cut s 4) isInQuotation buffer
                                     | startWith "false" s && isInQuotation !! index == False = ["false"] ++ scanner (index + 5) (cut s 5) isInQuotation buffer
                                     | startWith "null" s && isInQuotation !! index == False = ["null"] ++ scanner (index + 4) (cut s 4) isInQuotation buffer
                                     | startWith "\"" s && isInQuotation !! index == False = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ "&quot;")
                                     | startWith "\\" s && isInQuotation !! index == True && startWith "\"" (cut s 1) = scanner (index + 2) (cut s 2) isInQuotation (buffer ++ "<span style=\"color:rgb(200, 0, 100)\">\\&quot;</span>")
                                     | startWith "\\" s && isInQuotation !! index == True && startWith "u" (cut s 1) = scanner (index + 6) (cut s 6) isInQuotation (buffer ++ "<span style=\"color:rgb(200, 0, 100)\">\\u" ++ (getSubstring s 2 5) ++ "</span>")
                                     | startWith "\\" s && isInQuotation !! index == True = scanner (index + 2) (cut s 2) isInQuotation (buffer ++ "<span style=\"color:rgb(200, 0, 100)\">\\" ++ (getSubstring s 1 1) ++ "</span>")
                                     | startWith "<" s && isInQuotation !! index == True = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ "&lt;")
                                     | startWith ">" s && isInQuotation !! index == True = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ "&gt;")
                                     | startWith "&" s && isInQuotation !! index == True = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ "&amp;")
                                     | startWith "'" s && isInQuotation !! index == True = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ "&apos;")
                                     | startWith "\"" s && isInQuotation !! index == True = [buffer ++ "&quot;"] ++ scanner (index + 1) (cut s 1) isInQuotation ""
                                     | isInQuotation !! index == True = scanner (index + 1) (cut s 1) isInQuotation (buffer ++ (getSubstring s 0 0))
                                     | isDigit [s !! 0] && s !! 0 /= 'e' && s !! 0 /= 'E' && isInQuotation !! index == False = [getSubstring s 0 ((numberOfDigit s) - 1)] ++ scanner (index + numberOfDigit s) (cut s (numberOfDigit s)) isInQuotation buffer
                                     | otherwise = scanner (index + 1) (cut s 1) isInQuotation buffer


makeIndent :: Handle -> Int -> IO()
makeIndent out 0 = return ()
makeIndent out num = do
  hPutStr out "\t"
  hPutStr out "<span>&emsp;</span>"
  makeIndent out (num - 1)


formatter :: Handle -> [String] -> Int -> Int -> [String] -> [String] -> IO() -- handle -> tokens -> index -> indent -> stack -> copy -> operations
formatter out [] index indent stack copy = hPutStrLn out "</span>"
formatter out (x: xs) index indent stack copy | x == "{" = do
  hPutStrLn out "<br />"
  makeIndent out indent
  hPutStrLn out "<span style=\"color:rgb(255, 0, 0)\">{</span><br />"
  formatter out xs (index + 1) (indent + 1) (push "{" stack) copy

                                         | x == "}" = do
  hPutStrLn out "<br />"
  makeIndent out (indent - 1)
  hPutStr out "<span style=\"color:rgb(255, 0, 0)\">}</span>"
  formatter out xs (index + 1) (indent - 1) (pop stack) copy

                                         | x == "[" = do
  hPutStr out " <span style=\"color:rgb(0, 255, 0)\">[</span>"
  formatter out xs (index + 1) indent (push "[" stack) copy
                             
                                         | x == "]" = do
  hPutStr out " <span style=\"color:rgb(0, 255, 0)\">]</span>"
  formatter out xs (index + 1) indent (pop stack) copy

                                         | x == "," && top stack == "{" = do
  hPutStr out "<span style=\"color:rgb(0, 0, 255)\">,</span>"
  hPutStrLn out "<br />"
  formatter out xs (index + 1) indent stack copy

                                         | x == "," = do
  hPutStr out "<span style=\"color:rgb(0, 0, 255)\">,</span>"
  formatter out xs (index + 1) indent stack copy

                                         | x == ":" = do
  hPutStr out " <span style=\"color:rgb(100, 100, 0)\">:</span>"
  formatter out xs (index + 1) indent stack copy

                                         | x == "true" = do
  hPutStr out " <span style=\"color:rgb(244, 164, 96)\">true</span>"
  formatter out xs (index + 1) indent stack copy

                                         | x == "false" = do
  hPutStr out " <span style=\"color:rgb(244, 164, 96)\">false</span>"
  formatter out xs (index + 1) indent stack copy

                                         | x == "null" = do
  hPutStr out " <span style=\"color:rgb(244, 164, 96)\">null</span>"
  formatter out xs (index + 1) indent stack copy

                                         | startWith "&quot;" x && top stack == "[" = do
  hPutStr out (" <span style=\"color:rgb(50, 150, 0)\">" ++ x ++ "</span>")
  formatter out xs (index + 1) indent stack copy

                                         | startWith "&quot;" x && top stack == "{" && index >= 1 && copy !! (index - 1) == ":" = do
  hPutStr out (" <span style=\"color:rgb(50, 150, 0)\">" ++ x ++ "</span>")
  formatter out xs (index + 1) indent stack copy

                                         | startWith "&quot;" x && top stack == "{" && index >= 1 && copy !! (index - 1) /= ":" = do
  makeIndent out indent
  hPutStr out (" <span style=\"color:rgb(50, 150, 0)\">" ++ x ++ "</span>")
  formatter out xs (index + 1) indent stack copy

                                         | isDigit [x !! 0] = do
  hPutStr out (" <span style=\"color:rgb(100, 0, 100)\">" ++ x ++ "</span>")
  formatter out xs (index + 1) indent stack copy

                                         | otherwise = do
  formatter out xs (index + 1) indent stack copy


main = do
    putStr "Please input the name of the json file: "
    fileName <- getLine
    json <- readFile fileName
    let reference = (inQuotation False False json)
    out <- openFile "json.html" WriteMode
    hPutStrLn out "<span style=\"font-family:monospace; white-space:pre\">"
    formatter out (scanner 0 json reference "") 0 0 [] (scanner 0 json reference "")
    hClose out
    putStrLn "Format transformation completed! Please check json.html!"
