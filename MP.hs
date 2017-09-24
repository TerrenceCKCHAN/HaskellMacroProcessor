module MP where

import System.Environment

type FileContents = String
type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp b xs
  = [k | (d , k) <- xs, b == d]



split :: [Char] -> String -> (String, [String])
split _ [] = ("", [""])
split ys (x : xs)
  | elem x ys = (x : ks, "" : s : sth)   
  | otherwise = (ks, (x : s) : sth)        
    where (ks, s : sth) = split ys xs 


-- To match the type [String] and to represent the characters (k' : (k : ks))
combine :: String -> [String] -> [String]
combine [] []             = [""]
combine [] (k : ks)       = (k : ks)
combine (x : xs) []       = [(x : xs)]
combine (x : xs) (k : ks) = (k : [x] : combine xs ks)


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []       = []
getKeywordDefs (x : xs) = (k, concat (combine ts ks)) : getKeywordDefs xs
    where ((t : ts) , (k : ks)) = split " " x
    


expand :: FileContents -> FileContents -> FileContents
expand text info = replaceWord text defs
    where
      (_, infoLines) = split "\n" info
      defs           = getKeywordDefs infoLines


replaceWord :: String -> KeywordDefs -> String
replaceWord text defs
  = concat $ combine sepsInText (mapWordList words)
  where
    (sepsInText, words) = split separators text
    mapWordList :: [String] -> [String]
    mapWordList []    = []
    mapWordList (s:sx)
      | s      == ""  = "" : mapWordList sx
      | head s == '$' = head (lookUp s defs) : mapWordList sx
      | otherwise     = s : mapWordList sx



main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")



