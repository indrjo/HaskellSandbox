
import Control.Monad       (liftM, forM_)
import Control.Conditional (ifM)
import Data.List.Extra     (trim)
import System.Environment  (getArgs)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)
import System.Process      (callCommand)

main :: IO ()
main = do
  files <- getArgs
  if null files
    then say "provide some file to parse! doing nothing..."
    else forM_ files $ \f -> ifM (doesFileExist f)
        (kalamares f)
        --(do { say $ "reading \'" ++ f ++ "\'..."; kalamares f })
        (say $ "\'" ++ f ++ "\' does not exist!")

-- kalamares data
data Kalamares = Base FilePath FilePath
               | Copy FilePath FilePath
               | Unknown String

-- turn a string into a kalamares data
toKal :: String -> Kalamares
toKal str
    | elem '@' str = let (p1, p2) = chop '@' str in Base p1 p2
    | elem '>' str = let (p1, p2) = chop '>' str in Copy p1 p2
    | otherwise    = Unknown str
  where
    chop :: Char -> String -> (String, String)
    chop c str = (trim a, trim . drop 1 $ b)
      where (a, b) = span (/= c) str

-- *** core functions ***
-- "kalamares" is the core function: takes a file and parses it.
kalamares :: FilePath -> IO ()
kalamares f = helper "" "" =<< getMeaningfulLinesOf f
  where
    -- This helper function is the actual parser. Takes a line at time, blows 
    -- it up with the "toKal" function and decide what to do.
    helper :: FilePath -> FilePath -> [String] -> IO ()
    helper _ _ []     = return ()
    helper p q (x:xs) = case toKal x of
        Base a b  -> helper a b xs
        Copy a b  -> do
            copy (p </> a) (q </> b)
            helper p q xs
        Unknown c -> do
            say $ "[ " ++ f ++ " ] \'" ++ c ++
                  "\': what do you want me to do here?"
            helper p q xs  
    -- Get all meaningful lines from a file. Here, "meaningful" stands for
    -- "neither blank line nor line which starts with '#'".
    getMeaningfulLinesOf :: FilePath -> IO [String]
    getMeaningfulLinesOf = liftM (filter isMeaningful . lines) . readFile
      where
        isMeaningful :: String -> Bool
        isMeaningful ""        = False
        isMeaningful ('#':_ )  = False
        isMeaningful (' ':str) = isMeaningful str
        isMeaningful _         = True

-- *** comunicate with the outside world ***
say :: String -> IO ()
say = putStrLn . (" *** " ++)

-- *** functions to re-implement ***
copy :: FilePath -> FilePath -> IO ()
copy a b = do
    callCommand $ "mkdir -p " ++ b
    callCommand $ "cp -ruv " ++ a ++ " " ++ b

-- *** function yet to implement ***
move :: FilePath -> FilePath -> IO ()
move = undefined

{-kalamares :: FilePath -> IO ()
kalamares f = helper "" "" 0 =<< getMeaningfulLinesOf f
  where
    -- This helper function is the actual parser. Takes a line at time, blows 
    -- it up with the "toKal" function and decide what to do.
    helper :: FilePath -> FilePath -> Int -> [String] -> IO ()
    helper _ _ _ []     = return ()
    helper p q n (x:xs) = case toKal x of
        Base a b  -> helper a b (n+1) xs
        Copy a b  -> do
            copy (p </> a) (q </> b)
            helper p q (n+1) xs
        Unknown c -> do
            say $ "[ " ++ f ++ " at line " ++ show n ++ " ] \'" ++ c ++
                  "\': what do you want me to do here?"
            helper p q (n+1) xs  
    -- Get all meaningful lines from a file. Here, "meaningful" stands for
    -- "neither blank line nor line which starts with '#'".
    getMeaningfulLinesOf :: FilePath -> IO [String]
    getMeaningfulLinesOf = liftM (filter isMeaningful . lines) . readFile
      where
        isMeaningful :: String -> Bool
        isMeaningful ""        = False
        isMeaningful ('#':_ )  = False
        isMeaningful (' ':str) = isMeaningful str
        isMeaningful _         = True-}

