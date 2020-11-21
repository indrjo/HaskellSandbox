
import Control.Monad (liftM)
import Control.Conditional (ifM)

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Extra (trim)

import System.Environment (getArgs)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Exit (die)

main :: IO ()
main = kalamares =<< getPathCouplesFrom =<< getValOfOrDie "--use"

kalamares :: [(FilePath, FilePath)] -> IO ()
kalamares [] = info "no action to perform!"
kalamares xs = mapM_ (uncurry copyToDir) xs
  where
    copyToDir :: FilePath -> FilePath -> IO ()
    copyToDir a b = do
        putStrLn  $ " " ++ a ++ " -> " ++ b 
        callCommand $
          "mkdir -p " ++ b ++ "; cp -ru " ++ a ++ " " ++ b 

getPathCouplesFrom :: FilePath -> IO [(FilePath, FilePath)] 
getPathCouplesFrom file = do
    start <- getValOfOrDie "--start"
    end   <- getValOfOrDie "--end"
    ifM (doesFileExist file)
      (liftM (map $ couple start end) (getMeaningfulLinesOf file))
      (die $ "\'" ++ file ++ "\' does not exist!")
  where
    couple :: FilePath -> FilePath -> String -> (FilePath, FilePath)
    couple p q = prefix p q . chop
      where
        prefix :: FilePath -> FilePath -> (FilePath, FilePath)
          -> (FilePath, FilePath)
        prefix p q (a, b) = (p </> a, q </> b)
        chop :: String -> (FilePath, FilePath)
        chop str = (h '>' str, k '>' str)
          where
            h, k :: Char -> String -> String
            h c = trim . takeWhile (/= c) 
            k c = trim . dropWhile (== c) . dropWhile (/= c)
    getMeaningfulLinesOf :: FilePath -> IO [String]
    getMeaningfulLinesOf = liftM (filter isMeaningful . lines) . readFile
      where
        isMeaningful :: String -> Bool
        isMeaningful ""        = False
        isMeaningful ('#':_ )  = False
        isMeaningful (' ':str) = isMeaningful str
        isMeaningful _         = True

-- OPTIONS HANDLING
isOpt :: String -> IO Bool
isOpt x = liftM (elem x) getArgs

getValOf :: String -> IO (Maybe String)
getValOf str = liftM (next str) getArgs
  where
    next :: Eq a => a -> [a] -> Maybe a
    next x = listToMaybe . dropWhile (== x) . dropWhile (/= x)

getValOfOrDie :: String -> IO String
getValOfOrDie key = do
    a <- getValOf key
    case a of
      Just b -> return b
      _      -> ifM (isOpt "--use")
                  (fatal $ "\'" ++ key ++ "\' needs a value!")
                  (fatal $ "provide \'" ++ key ++ "\' with an option!")

-- MESSAGES FROM THE PROGRAM
info :: String -> IO ()
info = putStrLn . (" INFO: " ++)

fatal :: String -> IO a
fatal = die . (" ERROR: " ++)
