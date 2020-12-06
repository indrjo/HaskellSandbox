#!/usr/bin/env runghc

import Data.List.Extra     (trim)
import Control.Conditional (ifM, unless)
import System.IO           (hPutStr, hPutStrLn, stderr)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)
import System.Process      (readCreateProcessWithExitCode, shell)
import System.Exit         (ExitCode(..))
import System.Environment  (getArgs)

-- kalamares is both the name of this progaram and of its core function. The
-- program kalamares is basically intended for command line usage. You can
-- simply issue
--   $ runghc kalamares.hs
-- and things work or compile this file and run it. The function kalamares
-- basically reads a file and does something.

-- The main is very small and quite simple: just pass the files you want this
-- program to parse via command-line as arguments.
main :: IO ()
main = mapM_ kalamares =<< getArgs

-- Blank lines are not taken into account; lines whose first non ' ' character
-- is '#' is considered a comment: they are ignored too.
isNotToParse :: String -> Bool
isNotToParse str = case dropWhile (== ' ') str of
    a:_ -> if a == '#' then True else False
    _   -> True

-- Kalamares data
data Kalamares = FT FilePath FilePath -- rebase action
               | CP FilePath FilePath -- copying things
               | MV FilePath FilePath -- moving things
               | IDK String           -- "I don't know"

-- Turn a string into a kalamares data.
toKalamares :: String -> Kalamares
toKalamares str
    | elem '&' str = let (p1, p2) = chop '&' str in FT p1 p2
    | elem '>' str = let (p1, p2) = chop '>' str in CP p1 p2
    | elem '@' str = let (p1, p2) = chop '@' str in MV p1 p2
    | otherwise    = IDK (trim str)
  where
    -- chop takes a string: an eventual piece which starts with '#' is left out
    -- and then the remaining part is chopped once a given char is met. 
    chop :: Char -> String -> (String, String)
    chop c = chopH . takeWhile (/= '#')
      where
        chopH :: String -> (String, String)
        chopH xs = (p xs, q xs)
          where
            p, q :: String -> String
            p = trim . takeWhile (/= c)
            q = trim . drop 1 . dropWhile (/= c)

-- kalamares is the heart of this program. It takes a file: if it exists, it is
-- parsed, otherwise you are said it does not exist.
kalamares :: FilePath -> IO ()
kalamares f = ifM (doesFileExist f)
    (parseLines . lines =<< readFile f)
    (warn $ "\'" ++ f ++ "\' does not exist!")
  where
    -- This following function is the actual parser. Takes a line at time, blows
    -- it up with the "kal" function and decide what to do with it.
    parseLines :: [String] -> IO ()
    parseLines = parseH 1 "" ""
      where
        parseH :: Int -> FilePath -> FilePath -> [String] -> IO ()
        parseH _ _ _ []     = return ()
        parseH n p q (x:xs)
            | isNotToParse x = parseH (n+1) p q xs 
            | otherwise = case toKalamares x of
                -- Change the bases.
                FT s t -> parseH (n+1) s t xs
                -- Copy something into something else.
                CP a b -> do exec $ "cp -ruv " ++ p </> a ++ " " ++ q </> b
                             parseH (n+1) p q xs
                -- Move something into something else.
                MV a b -> do exec $ "mv " ++ p </> a ++ " " ++ q </> b
                             parseH (n+1) p q xs
                -- "I don't know" lines: kalamares alerts you whether there is
                -- a line it hasn't fully understood; you are also told where
                -- ambiguous lines lie. 
                IDK c  -> do warn $ "[" ++ f ++ " at line " ++ show n ++ "]"
                                    ++ " \'" ++ c ++ "\': "
                                    ++ "what do you expect me to do?"
                             parseH (n+1) p q xs
          where
            -- exec is the wrapper of unix commands used here.
            exec :: String -> IO ()
            exec cmd = do
                -- Say which command the program is executing.
                putStrLn $ "[running] \'" ++ cmd ++ "\'... "
                -- Get exit code and error messages may arise.
                (exitCode, _, errMsg) <-
                    readCreateProcessWithExitCode (shell cmd) ""
                -- If a non zero exit code is thrown, simply inform the user
                -- with the error message comes from that command and go ahead:
                -- never stop because those errors. 
                unless (exitCode == ExitSuccess)
                    (hPutStr stderr . h $ errMsg)
              where
                h :: String -> String
                h str = "[" ++ f ++ " at line " ++ show n ++ "]"
                        ++ (drop 1 . dropWhile (/= ':') $ str)
                
-- warnings
warn :: String -> IO ()
warn str = hPutStrLn stderr $ " *** " ++ str

