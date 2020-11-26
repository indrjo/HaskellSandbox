
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
    then
      putStrLn " *** provide some file to parse!"
    else
      forM_ files $ \f -> do
        existence <- doesFileExist f
        if existence
          then do
            putStrLn $ " *** reading \'" ++ f ++ "\'..."
            kalamares f
          else
            putStrLn $ " *** \'" ++ f ++ "\' does not exist!"

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
            putStrLn $ " *** \'" ++ c ++ "\': what do you want me to do?"
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

-- *** functions to re-implement ***
copy :: FilePath -> FilePath -> IO ()
copy a b = do
    callCommand $ "mkdir -p " ++ b
    callCommand $ "cp -ruv " ++ a ++ " " ++ b

-- *** function yet to implement ***
move :: FilePath -> FilePath -> IO ()
move = undefined

