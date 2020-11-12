
import Control.Monad              (liftM, liftM2)
import Control.Conditional        (ifM, cond)

import Data.Maybe                 (listToMaybe, fromMaybe)
import Data.List.Extra            (trim)
import Data.Time.Clock            (getCurrentTime, utctDay)
import Data.Time.Calendar         (toGregorian)

import System.Environment         (getArgs)
import System.Environment.FindBin (getProgPath)
import System.FilePath            ((</>), (<.>))
import System.Directory           (doesFileExist)
import System.Exit                (die)

main :: IO ()
main = parseArgs [ ("--n", addNewDataTo =<< getDataFile)
                 , ("--s", printTotalAmountOf =<< getDataFile)
                 ] (die "no option provided!")

-- *** options handling ***
parseArgs :: [(String, IO a)] -> IO a -> IO a
parseArgs []             def = def
parseArgs ((str, io):xs) def = ifM (isOpt str) io (parseArgs xs def)
  where
    isOpt :: String -> IO Bool
    isOpt str = liftM (elem str) getArgs 

getCmdLnValOf :: String -> IO (Maybe String)
getCmdLnValOf str = liftM (listToMaybe . after str) getArgs

-- *** getting files to store datas ***
getDataFile :: IO FilePath
getDataFile = liftM2 (</>) getDefDir (liftM (<.> "dat") getYear)
  where
    getDefDir :: IO FilePath
    getDefDir = liftM2 fromMaybe
                  (liftM (</> "datas") getProgPath)
                  (getCmdLnValOf "--d")
    getYear :: IO String
    getYear = liftM2 fromMaybe getThisYear (getCmdLnValOf "--y")
      where
        getThisYear :: IO String
        getThisYear = do
            (y, _, _) <- liftM (toGregorian . utctDay) getCurrentTime
            return (show y)         

-- *** adding new datas ***
addNewDataTo :: FilePath -> IO ()
addNewDataTo file = appendFile file =<< liftM2 (++) newLn getLine 
  where
    newLn :: IO String
    newLn = ifM (doesFileExist file) (return "\n") (return "")

-- *** getting the total amount ***
printTotalAmountOf :: FilePath -> IO ()
printTotalAmountOf file = print =<< liftM sum (getEurosFrom file)
  where
    getEurosFrom :: FilePath -> IO [Double]
    getEurosFrom = liftM (map (read . trim . after '€')) . getLines
      where
        getLines :: FilePath -> IO [String]
        getLines = liftM (filter isGood . lines) . readFile
          where
            isGood :: String -> Bool
            isGood ""       = False
            isGood ('#':_)  = False
            isGood (' ':xs) = isGood xs
            isGood _        = True

-- *** small, but crucial functions ***
after :: Eq a => a -> [a] -> [a]
after c = dropWhile (== c) . dropWhile (/= c)

