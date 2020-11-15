
{-
  DESCRIPTION
  ------------------------------------------------------------------------------
  This is a small Haskell program intended for performing the following boring
  action: copy a bunch of things spreaded all over your disk into a single
  contained place (e.g. an usb pendrive).
  
  
  BASIC USAGE
  ------------------------------------------------------------------------------
  Basically:
  
    1. put a copy of this program inside the location you want to gather things
       in: such place from now on is the "base directory" for us;
    2. there you have to write a plain-text file called "to.here" stuctured made
       of lines of shape
         A > B
       that for the program means: "copy A inside the directory B in the base
       directory; if B does not exist, create it, and then make copies".
  
  For example, issuing from commandline
    $ runghc this-program
  what you would expect happens.
  
  
  FURTHER FEATURES [Yet to come...]
  ------------------------------------------------------------------------------
  
  * add the possibility *move* things, intead of simply copying them;
  * why does one have to put a copy of that program inside a directory to copy
    things in it?
  * a better name.
-}

import Data.List.Extra            (trim)
import Control.Monad              (liftM)
import Control.Conditional        (unlessM)
import System.Environment         (getArgs)
import System.Environment.FindBin (getProgPath)
import System.FilePath            ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Process             (callCommand)

-- *** main ***
main :: IO ()
main = mapM_ (uncurry toHere) =<< getToHerePairs

-- *** core functions ***

-- copy something (first argument) into a dir (second argument)
-- which is contained in the directory where this program lies 
toHere :: FilePath -> FilePath -> IO ()
toHere p1 p2 = copy p1 =<< liftM (</> p2) getProgPath
  where
    copy :: FilePath -> FilePath -> IO ()
    copy a b = do
        createDirectoryIfMissing True b
        exec $ "cp -ru " ++ a ++ " " ++ b

-- get a list of pairs: for each of them, components are used
-- as arguments of the copy function
getToHerePairs :: IO [(FilePath, FilePath)]
getToHerePairs = liftM (map chop) getToHereLines
  where
    -- get from a line source and target
    chop :: String -> (FilePath, FilePath)
    chop str = (source str, target str)
      where
        source, target :: String -> FilePath
        source = trim . takeWhile (/= '>')
        target = trim . dropWhile (== '>') . dropWhile (/= '>')
    -- gather the lines of the file "to.here" which lies in
    -- the same directory where the program lies
    getToHereLines :: IO [String]
    getToHereLines = getLinesOf =<< getToHereFilePath
      where
        getLinesOf :: FilePath -> IO [String]
        getLinesOf = liftM (filter useful . lines) . readFile
          where
            useful :: String -> Bool
            useful ""       = False
            useful ('#':_)  = False
            useful (' ':xs) = useful xs
            useful _        = True
        getToHereFilePath :: IO FilePath
        getToHereFilePath = liftM (</> "to.here") getProgPath

-- say whether a string is a commandline options
isOpt :: String -> IO Bool
isOpt x = liftM (elem x) getArgs

-- issue a system command and tell what you are doing
exec :: String -> IO ()
exec cmd = do
    putStrLn $ " >>> " ++ cmd ++ "..."
    unlessM (isOpt "--preview")
      (callCommand cmd)

