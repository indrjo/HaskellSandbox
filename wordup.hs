
import Data.Char
import Control.Conditional
import Text.Regex.PCRE
import Text.Regex
import System.IO
import System.FilePath
import System.Directory
import System.Environment.FindBin
import System.Exit

main :: IO ()
main = do
    chdir "dictionary"
    meaning <- lookForMeaningsOf =<< getInput "insert a word"
    case meaning of
        Just m -> (putStrLn . rep "\\s*#\\s*" "\n") m
        _      -> die "nothing found!"


-- *** Core functions ***
        
lookForMeaningsOf :: String -> IO (Maybe String)
lookForMeaningsOf word = withFile (wrdOf word) ReadMode (lookForLoop word)
  where
    -- Get the file containing a word, that is the one that has the same 
    -- first letter of the word.
    wrdOf :: String -> FilePath
    wrdOf str = [head str] <.> "wrd"
    -- The actual searcher.
    lookForLoop :: String -> Handle -> IO (Maybe String)
    lookForLoop str hdl = ifM (hIsEOF hdl) (return Nothing) $ do 
        line <- hGetLine hdl
        if line =~ ("^\\s*" ++ str ++ "\\s*:")
          then return (Just (rep "^[^:]*:\\s*|\\s*$" "" line))
          else lookForLoop str hdl


-- *** Helper functions ***

-- Always change directory inside the one where lies the main program!
chdir :: FilePath -> IO ()
chdir dir = do
    here <- fmap (</> dir) getProgPath
    ifM (doesDirectoryExist here) (setCurrentDirectory here)
      (die $ "no directory " ++ here ++ "!")

-- Get informations from users via standard input. The user is forced to type 
-- something (empty answers are refused as well as blank ones): the function 
-- below is insistent, that is if you do not comply such rule, you will be
-- asked again to insert something. The IO string returned has the initial and
-- trailing spaces removed and all characters turned into lowercase.
getInput :: String -> IO String
getInput msg = do
    -- question time...
    putStr (msg ++ ": ") >> hFlush stdout
    -- answer time...
    input <- fmap (map toLower . rep "^\\s*|\\s*$" "") getLine
    -- decision time...
    if null input then getInput msg else return input

-- Regexp replacement.
rep :: String -> String -> String -> String
rep pat sub = flip (subRegex (mkRegex pat)) sub

