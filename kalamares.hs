
import Control.Monad       (liftM)
import Control.Conditional (ifM)
import Data.Maybe          (listToMaybe, fromMaybe)
import Data.List.Extra     (trim)
import System.Environment  (getArgs)
import System.Process      (callCommand)
import System.Directory    (doesFileExist)
import System.FilePath     ((</>))
import System.Exit         (die)

main :: IO ()
main = do
    this <- getValOfOrDie "--use"  
    ifM (doesFileExist this)
      (makeCopies =<< getCouplesFrom this)
      (fatal $ "\'" ++ this ++ "\' does not exist!")

-- CORE FUNCTIONS
type FilePath2 = (FilePath, FilePath)

makeCopies :: [FilePath2] -> IO ()
makeCopies [] = info "no action to perform!"
makeCopies xs = mapM_ (uncurry copyToDir) xs
  where
    copyToDir :: FilePath -> FilePath -> IO ()
    copyToDir a b = do
        putStrLn  $ " " ++ a ++ " -> " ++ b ++ " ..."
        callCommand $
          "mkdir -p " ++ b ++ "; cp -ru " ++ a ++ " " ++ b 

getCouplesFrom :: FilePath -> IO [FilePath2] 
getCouplesFrom file = do
    a <- getValOfOrDie "--start"
    b <- getValOfOrDie "--end"
    liftM (map $ addPrefixes a b) (getMeaningfulLinesOf file)
  where
    addPrefixes :: FilePath -> FilePath -> String -> FilePath2
    addPrefixes p q = (\(x, y) -> (p </> x, q </> y)) . chop
      where
        chop :: String -> FilePath2
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

getValOfOrDie :: String -> IO String
getValOfOrDie key = do
    a <- getValOf key
    case a of
      Just b -> return b
      _      -> ifM (isOpt "--use")
                  (fatal $ "\'" ++ key ++ "\' needs a value!")
                  (fatal $ "provide \'" ++ key ++ "\' with an option!")
  where
    getValOf :: String -> IO (Maybe String)
    getValOf x = liftM (next x) getArgs
      where
        next :: Eq s => s -> [s] -> Maybe s
        next c = listToMaybe . dropWhile (== c) . dropWhile (/= c)

-- MESSAGES FROM THE PROGRAM
info :: String -> IO ()
info = putStrLn . (" INFO: " ++)

fatal :: String -> IO a
fatal = die . (" ERROR: " ++)

