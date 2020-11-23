
import Control.Monad       (liftM)
import Control.Conditional (ifM)
import Data.List.Extra     (trim)
import System.Environment  (getArgs)
import System.Process      (callCommand)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)

main :: IO ()
main = mapM_ kalamares =<< getArgs

kalamares :: FilePath -> IO ()
kalamares file = ifM (doesFileExist file)
    (parseStr "" "" =<< getKalLinesOf file)
    (putStrLn $ file ++ " does not exist!")
  where
    parseStr :: String -> String -> [String] -> IO ()
    parseStr _ _ [] = return ()
    parseStr a b (x:xs)
        | elem '@' x = let (c, d) = chop '@' x in
                         parseStr c d xs
        | elem '>' x = let (p, q) = chop '>' x in do
                         copyToDir (a </> p) (b </> q)
                         parseStr a b xs
        | otherwise = parseStr a b xs

copyToDir :: FilePath -> FilePath -> IO ()
copyToDir a b = do
    callCommand $ "mkdir -p " ++ b
    callCommand $ "cp -ruv " ++ a ++ " " ++ b

getKalLinesOf :: FilePath -> IO [String]
getKalLinesOf = liftM (filter isMeaningful . lines) . readFile
  where
    isMeaningful :: String -> Bool
    isMeaningful ""        = False
    isMeaningful ('#':_ )  = False
    isMeaningful (' ':str) = isMeaningful str
    isMeaningful _         = True

-- HELPER FUNCTIONS
chop :: Char -> String -> (String, String)
chop c str = let (a, b) = span (/= c) str in
               (trim a, trim . drop 1 $ b)

