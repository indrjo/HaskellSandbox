
import Data.Char
import Control.Conditional
import Text.Regex.PCRE
import Text.Regex
import System.IO
import System.FilePath
import System.Directory
import System.Environment.FindBin
import System.Exit


-- *** Main ***

main :: IO ()
main = do
    chdir . (</> "dict") =<< getProgPath
    meaning <- translate =<< input "insert a word"
    case meaning of
        Just a -> (putStrLn . rep "\\s*#\\s*" "\n") a
        _      -> die "nothing found!"


-- *** Core functions ***

translate :: String -> IO (Maybe String)
translate word = let wordFile = [head word] <.> "wrd" in
    ifM (doesFileExist wordFile)
      (withFile wordFile ReadMode (loop word))
      (die $ wordFile ++ " does not exist!")
  where
    loop :: String -> Handle -> IO (Maybe String)
    loop str hdl = ifM (hIsEOF hdl) (return Nothing) $ do 
        line <- hGetLine hdl
        if line =~ ("^\\s*" ++ str ++ "\\s*:")
          then return (Just (rep "^[^:]*:\\s*|\\s*$" "" line))
          else loop str hdl


-- *** Helper functions ***

-- Insert something from standard input.
input :: String -> IO String
input msg = do
    putStr (msg ++ ": ") >> hFlush stdout
    ans <- fmap (map toLower . rep "^\\s*|\\s*$" "") getLine
    if null ans then input msg else return ans

-- Regexp replacement.
rep :: String -> String -> String -> String
rep pat sub = flip (subRegex (mkRegex pat)) sub

-- Change directory.
chdir :: FilePath -> IO ()
chdir here = ifM (doesDirectoryExist here)
    (setCurrentDirectory here)
    (die $ here ++ " does not exist!")

