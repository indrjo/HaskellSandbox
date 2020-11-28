
{-
  *** Description yet to come... ***
-}

import Control.Monad       (liftM)
import Control.Conditional (ifM)
import Data.List.Extra     (trim)
import System.Environment  (getArgs)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)
import System.Process      (callCommand)

main :: IO ()
main = mapM_ kalamares =<< getArgs

-- kalamares data
data Kalamares = NotToParse
               | FT FilePath FilePath
               | CP FilePath FilePath
               | MV FilePath FilePath
               | IDK String

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

-- *** core functions ***
-- "kalamares" is the core function: takes a file and parses it.
kalamares :: FilePath -> IO ()
kalamares f = ifM (doesFileExist f)
    (kalamaresH =<< liftM lines (readFile f))
    (say $ "\'" ++ f ++ "\' does not exist!")
  where
    -- This helper function is the actual parser. Takes a line at time, blows 
    -- it up with the "toKal" function and decide what to do.
    kalamaresH :: [String] -> IO ()
    kalamaresH = helper "" "" 1
      where
        helper :: FilePath -> FilePath -> Int -> [String] -> IO ()
        helper _ _ _ []     = return ()
        helper p q n (x:xs) = case kal x of
            NotToParse -> helper p q (n+1) xs
            FT p' q'   -> helper p' q' (n+1) xs
            CP a b     -> do { cp (p </> a) (q </> b); helper p q (n+1) xs }
            MV a b     -> do { mv (p </> a) (q </> b); helper p q (n+1) xs }
            IDK c      -> say $ "[" ++ f ++ ", " ++ show n  ++ "] \'" ++ c ++
                                "\': what do you expect me to do with that?"

-- *** comunicate with the outside world ***
say :: String -> IO ()
say = putStrLn . (" *** " ++)

-- *** functions to re-implement ***
cp :: FilePath -> FilePath -> IO ()
cp a b = do
    callCommand $ "mkdir -p " ++ b
    callCommand $ "cp -ruv " ++ a ++ " " ++ b

-- *** function yet to implement ***
mv :: FilePath -> FilePath -> IO ()
mv a b = callCommand $ "mv " ++ a ++ " " ++ b

