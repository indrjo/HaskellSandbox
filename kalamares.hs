#!/usr/bin/runghc

{-# LANGUAGE Strict #-}

-- kalamares is both the name of this progaram and of its core function; also,
-- there is a type with the same name written with capital 'k'. The program 
-- kalamares is basically intended for command line usage. You can simply issue
--   $ runghc kalamares.hs
-- and things work or compile this file and run it. The function kalamares
-- basically reads a file and does something.

-- *** Modules used ***

-- Functions deal with condtionals. 
import Control.Conditional
    (ifM, unless, unlessM, cond)

-- System modules.
import System.IO
    (hPutStrLn, stderr, Handle, IOMode(..), withFile, hIsEOF, hGetLine)
import System.FilePath
    ((</>))
import System.Directory
    (doesFileExist)
import System.Process
    (readCreateProcessWithExitCode, shell)
import System.Exit
    (ExitCode(..), die)
import System.Environment
    (getArgs)

-- Regex modules for regex stuff.
import Text.Regex.PCRE
    ((=~))
import Text.Regex
    (mkRegex, splitRegex, subRegex)

-- WARNING: you may need to 'cabal install regex-pcre' first.

-- *** The main ***

-- The main is very small and quite simple: just pass the files you want this
-- program to parse via command-line as arguments.
main :: IO ()
main = do
    -- All command line arguments ought be files to parse by kalamares. Anyway,
    -- if something does not correspond to any file, kalamares informs you.
    -- Files you want to parse should be written in a comprehensible way: a well
    -- written file is up to users!
    kals <- getArgs
    if null kals
      -- Throw an exit-failure code if no file is given to kalamares. That
      -- feature may be useful if the program is embedded in another one. 
      then die "no file provided: doing nothing..."
      -- The core action of the program.
      else mapM_ kalamares kals

-- *** Things behind the scenes ***
    
-- Kalamares data.
data Kalamares = FT FilePath FilePath -- rebase action
               | CP FilePath FilePath -- copying things
               | MV FilePath FilePath -- moving things
               | IDK String           -- "I don't know"

-- All and only the recognised separators.
seps :: String
seps = "&>@"

-- Turn a string into a kalamares data. 
toKalamares :: String -> Kalamares
toKalamares str = case (spl seps . rep "#.*$") str of
    a:b:_ -> let a' = rep "^\\s*|\\s*$" a
                 b' = rep "^\\s*|\\s*$" b
             in cond [ (elem '&' str, FT a' b')
                     , (elem '>' str, CP a' b')
                     , (elem '@' str, MV a' b') ]
    [c]   -> let c' = rep "^\\s*|\\s*$" c in IDK c'
    -- Call on an empty string. That case should never occur though.
    -- Anyway, you are alerted if that happens.
    []    -> error "application on an empty string!"

-- kalamares is the very core function of this program.
kalamares :: FilePath -> IO ()
kalamares kalF = ifM (doesFileExist kalF)
    -- If the file given to kalamares exists, parse it.
    (withFile kalF ReadMode _kalamares)
    -- Otherwise, alert users it does not exist.
    (warn $ kalF ++ " does not exist!")
  where
    -- The function below is the actual parser: it takes one line at time from
    -- the file to parse and decides what to do with it.
    _kalamares :: Handle -> IO ()
    _kalamares = loop 1 "" ""
      where 
        loop :: Int -> FilePath -> FilePath -> Handle -> IO ()
        loop n p q hdl = unlessM (hIsEOF hdl) $ do
            ln <- hGetLine hdl
            -- Blank lines or lines whose first non-' ' is '#' are not taken 
            -- into any account. Use Blank lines better spatial oragnization of
            -- your files, and lines starting with '#' as your own notes.
            if ln =~ "^\\s*$" || ln =~ "^\\s*#"
              then loop (n+1) p q hdl
              -- Everything else is potentially parsable: blow up each line into
              -- a kalamares data.
              else case toKalamares ln of
                FT s t -> loop (n+1) s t hdl
                CP a b -> do
                    exec kalF n $ "cp -ru " ++ p </> a ++ " " ++ q </> b
                    loop (n+1) p q hdl
                MV a b -> do
                    exec kalF n $ "mv " ++ p </> a ++ " " ++ q </> b
                    loop (n+1) p q hdl
                IDK c  -> do
                    warn $ wStart kalF n ++ "\'" ++ c ++ "\': "
                           ++ "what do you expect me to do?"
                    loop (n+1) p q hdl

-- Unix command wrapper.
exec :: FilePath -> Int -> String -> IO ()
exec file n cmd = do
    -- Say which command the program is executing.
    putStrLn $ " >>> " ++ cmd
    -- Get exit code and error messages may arise.
    (exitCode, _, errMsg) <- run cmd
    -- If a non zero exit code is thrown, simply inform users with
    -- the error message comes from that command and go ahead: 
    -- never stop because those errors.
    unless (exitCode == ExitSuccess) $
      (warn . (++) (wStart file n) . rep "\\s*$") errMsg

-- Warn users. This kind of communication occurs via standard error channel.
warn :: String -> IO ()
warn = hPutStrLn stderr

-- Warning starter.
wStart :: FilePath -> Int -> String
wStart f n = "[" ++ f ++ ", line " ++ show n ++ "] "

-- Run system commands.
run :: String -> IO (ExitCode, String, String)
run = flip readCreateProcessWithExitCode "" . shell

-- Removing parts from strings using regular expressions.
rep :: String -> String -> String
rep pat = flip (subRegex (mkRegex pat)) ""

-- Splitting.
spl :: String -> String -> [String]
spl pat = splitRegex (mkRegex $ "\\s*[" ++ pat ++ "]\\s*")

