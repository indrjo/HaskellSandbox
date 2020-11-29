
{- Description yet to come... -}

import Control.Conditional (ifM)
import Data.List.Extra     (trim)
import System.Environment  (getArgs)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)
import System.Process      (callCommand)

main :: IO ()
main = mapM_ kalamares =<< getArgs

-- Kalamares data
data Kalamares = NotToParse           -- things not to parse
               | FT FilePath FilePath -- rebase action
               | CP FilePath FilePath -- copying things
               | MV FilePath FilePath -- moving things
               | IDK String           -- "I don't know"

-- *** core functions ***

-- kalamares is the heart of this program. It takes a file: if it exists, it
-- is parsed, otherwise you are said it does not exist.
-- Actually, no error or failure-exit-code is thrown, you are simply alerted 
-- through the standard output channel. Mind that feature!
kalamares :: FilePath -> IO ()
kalamares f = ifM (doesFileExist f) (parseLines =<< getLinesOf f)
                  (say $ "\'" ++ f ++ "\' does not exist!")
  where
    -- This following function is the actual parser. Takes a line at time, 
    -- blows it up with the "kal" function and decide what to do with it.
    parseLines :: [String] -> IO ()
    parseLines = parseH "" "" 1
      where
        parseH :: FilePath -> FilePath -> Int -> [String] -> IO ()
        parseH _ _ _ []     = return ()
        parseH p q n (x:xs) = case kal x of
            -- If a line is not to parse, go ahead. Here "lines not to parse"
            -- are blank lines or comments (a comment is exactly whatever piece
            -- of line starts with '#').
            NotToParse -> parseH p q (n+1) xs
            -- Change the bases.
            FT p' q'   -> parseH p' q' (n+1) xs
            -- Copy something into something else.
            CP a b     -> do
                cp (p </> a) (q </> b)
                parseH p q (n+1) xs
            -- Move something into something else.
            MV a b     -> do
                mv (p </> a) (q </> b)
                parseH p q (n+1) xs
            -- "I don't know" lines: kalamares alerts you whether there is
            -- a line it hasn't fully understood. You are also reported where
            -- ambiguous lines lie (the "*.kal" file and the line, which is
            -- nice if several files are given at once to kalamares). 
            IDK c      -> do 
                say $ "[" ++ f ++ ", " ++ show n  ++ "] \'" ++ c ++
                      "\': what do you expect me to do with that?"
                parseH p q (n+1) xs
    -- That function takes a filepath and throws out the IO list of its lines.
    getLinesOf :: FilePath -> IO [String]
    getLinesOf = fmap lines . readFile    

-- turn a string into a kalamares data
kal :: String -> Kalamares
kal str
    | isNotToParse str = NotToParse
    | elem '&' str     = let (p1, p2) = chop '&' str in FT p1 p2
    | elem '>' str     = let (p1, p2) = chop '>' str in CP p1 p2
    | elem '@' str     = let (p1, p2) = chop '@' str in MV p1 p2
    | otherwise        = IDK (trim str)
  where
    isNotToParse :: String -> Bool
    isNotToParse str = case dropWhile (== ' ') str of
        (x:_) -> if x == '#' then True else False
        []    -> True
    chop :: Char -> String -> (String, String)
    chop c str = (trim a, trim . drop 1 $ b)
      where
        (a, b) = span (/= c) . takeWhile (/= '#') $ str

-- communicate with the outside world
say :: String -> IO ()
say = putStrLn . (" *** " ++)

-- *** unix-ish functions ***

-- copy
cp :: FilePath -> FilePath -> IO ()
cp a b = do
    callCommand $ "mkdir -p " ++ b
    callCommand $ "cp -ruv " ++ a ++ " " ++ b

-- move
mv :: FilePath -> FilePath -> IO ()
mv a b = callCommand $ "mv " ++ a ++ " " ++ b

