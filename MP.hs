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
combine [] [] = [""]
combine [] (k : ks) = (k : ks)
combine (x : xs) [] = [(x : xs)]
combine (x : xs) (k : ks) = (k : [x] : combine xs ks)


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []       = []
getKeywordDefs (x : xs) = (k, concat (combine ts ks)) : getKeywordDefs xs
    where ((t : ts) , (k : ks)) = split " " x
    


expand :: FileContents -> FileContents -> FileContents
expand text info = replaceWord text defs
    where
      (_, infoLines) = split "\n" info
      defs = getKeywordDefs infoLines


replaceWord :: String -> KeywordDefs -> String
replaceWord text defs
  = concat $ combine sepsInText (mapWordList words)
  where
    (sepsInText, words) = split separators text
    mapWordList :: [String] -> [String]
    mapWordList [] = []
    mapWordList (s:sx)
      | s == "" = "" : mapWordList sx
      | head s == '$' = head (lookUp s defs) : mapWordList sx
      | otherwise = s : mapWordList sx



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











--Rough work
-- /////////////////////////////
--split [] []       = ([], [])
--split [] (y : ys) = ([], (y : ys))
--split (x : xs) [] = ([], [])
--split (x : xs) (y : ys) 
--  | ys == []      = []
--  | (x : xs) == y = ((x : split (x : xs) ys), (y : ys', ys'')) 
--      where (ys', ys'') = split (x : xs) ys
--if y is in x : xs then break the string and create an empty string afterwards 
--elem y (x : xs) = split 
-- | x == y = 
--[x | x <- (x : xs), y <- (y : ys), x == y]
--otherwise = (x : xs', xs'')
--x == x' = split xs
--  | otherwise = 

--  expand (x : xs) (y : ys)
--  | t == p = ps : expand ts (y:ys)
--  | otherwise = expand concat  
--      where (p, ps) = getKeywordDefs (y : ys)
--            (t, ts) = (combine (split separators (x : xs)))


{-
expand text info = replaceWord text defs
    where
      (_, infoLines) = split "\n" info
      defs = getKeywordDefs infoLines
 
--  expand (x : xs) (y : ys)
--  | t == p = ps : expand ts (y:ys)
--  | otherwise = expand concat  
--      where (p, ps) = getKeywordDefs (y : ys)
--            (t, ts) = (combine (split separators (x : xs)))



-- You may wish to uncomment and implement this helper function
-- when implementing expand

replaceWord :: String -> KeywordDefs -> String
replaceWord text defs
  = concat $ combine sepsInText (mapWordList words)
  where
    (sepsInText, words) = split separators text
    mapWordList :: [String] -> [String]
    mapWordList [] = []
    mapWordList (s:sx)
      | s == "" = "" : mapWordList sx
      | head s == '$' = head (lookUp s defs) : mapWordList sx
      | otherwise = s : mapWordList sx
-}
{-
replaceWord :: String -> KeywordDefs -> [String]
replaceword [] _ = []
replaceWord input@(x : xs) defs
  | head t == '$' = head (lookup t defs) : replaceWord ts
  | otherwise   = t : replaceWord ts
  where
    (t : ts) = uncurry combine (split separators input)
-}


