#!/usr/bin/env runghc

import Control.Conditional (ifM, unless, cond)
import System.IO           (hPutStrLn, stderr)
import System.FilePath     ((</>))
import System.Directory    (doesFileExist)
import System.Process      (readCreateProcessWithExitCode, shell)
import System.Exit         (ExitCode(..), die)
import System.Environment  (getArgs)
import Text.Regex.PCRE     ((=~))
import Text.Regex          (mkRegex, splitRegex, subRegex)

-- kalamares is both the name of this progaram and of its core function. The
-- program kalamares is basically intended for command line usage. You can
-- simply issue
--   $ runghc kalamares.hs
-- and things work or compile this file and run it. The function kalamares
-- basically reads a file and does something.

-- The main is very small and quite simple: just pass the files you want this
-- program to parse via command-line as arguments.
main :: IO ()
main = ifM (fmap null getArgs)
    (die "no file provided! doing nothing...")
    (mapM_ kalamares =<< getArgs)

-- Kalamares data
data Kalamares = FT FilePath FilePath -- rebase action
               | CP FilePath FilePath -- copying things
               | MV FilePath FilePath -- moving things
               | IDK String           -- "I don't know"

-- Recognised separators
seps :: String
seps = "&>@"

-- Turn a string into a kalamares data.
toKalamares :: String -> Kalamares
toKalamares str = case (spl seps . rep "#.*$") str of
    a:b:_ -> let
                a' = rep "^\\s*|\\s*$" a
                b' = rep "^\\s*|\\s*$" b
             in
                cond [ (elem '&' str, FT a' b')
                     , (elem '>' str, CP a' b')
                     , (elem '@' str, MV a' b') ]
    c:[]  -> let c' = rep "^\\s*|\\s*$" c in IDK c'
    -- Call on an empty string. That case should never occur though. Anyway,
    -- you are alerted if that happens.
    []    -> error "toKal applied on an empty string!"

-- kalamares is the heart of this program. It takes a file: if it exists, it is
-- parsed, otherwise you are said it does not exist.
kalamares :: FilePath -> IO ()
kalamares f = ifM (doesFileExist f)
    (parse . lines =<< readFile f)
    (warn $ f ++ " does not exist!")
  where
    -- The following function is the actual parser: it takes one string at time
    -- and decides what to do with it.
    parse :: [String] -> IO ()
    parse = parseH 1 "" ""
      where
        parseH :: Int -> FilePath -> FilePath -> [String] -> IO ()
        parseH _ _ _ []      = return ()
        parseH n p q (x:xs)
            | isNotToParse x = parseH (n+1) p q xs
            | otherwise      = case toKalamares x of
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
                IDK c  -> do warn $ wStart ++ "\'" ++ c ++ "\': "
                                    ++ "what do you expect me to do?"
                             parseH (n+1) p q xs
          where
            -- Blank lines are not taken into account. Lines whose first non
            -- blank char is '#' are ignored as well: use them as comments.
            isNotToParse :: String -> Bool
            isNotToParse str = str =~ "^\\s*#" || str =~ "^\\s*$"
            -- Every warning should start with the piece [<file>, line <int>]
            wStart :: String
            wStart = "[" ++ f ++ ", line " ++ show n ++ "] "
            -- exec is the wrapper of unix commands used here.
            exec :: String -> IO ()
            exec cmd = do
                -- Say which command the program is executing.
                putStrLn $ "[running] " ++ cmd
                -- Get exit code and error messages may arise.
                (exitCode, _, errMsg) <- run cmd
                -- If a non zero exit code is thrown, simply inform users with
                -- the error message comes from that command and go ahead: 
                -- never stop because those errors.
                unless (exitCode == ExitSuccess)
                    (warn . (++) wStart . rep "\\s*$" $ errMsg)

-- Warn users.
warn :: String -> IO ()
warn = hPutStrLn stderr

-- Removing parts from strings using regular expressions.
rep :: String -> String -> String
rep pat = flip (subRegex (mkRegex pat)) ""

-- Splitting
spl :: String -> String -> [String]
spl pat = splitRegex (mkRegex $ "\\s*[" ++ pat ++ "]\\s*")

-- Run system commands.
run :: String -> IO (ExitCode, String, String)
run = flip readCreateProcessWithExitCode "" . shell

