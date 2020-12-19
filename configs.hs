
module Configs (cfgValOf) where

import Control.Conditional (ifM)
import System.IO (withFile, IOMode(..), Handle(..), hGetLine, hIsEOF)
import Text.Regex.PCRE ((=~))
import Text.Regex (mkRegex, subRegex)

cfgValOf :: FilePath -> String -> IO (Maybe String)
cfgValOf cfgFile key = withFile cfgFile ReadMode (cfgLoop key)
  where
    cfgLoop :: String -> Handle -> IO (Maybe String)
    cfgLoop str hdl = ifM (hIsEOF hdl) (return Nothing) $ do
        ln <- fmap (rep "^\\s*|\\s*$" . rep "\\s*#.*$") (hGetLine hdl)
        if ln =~ ("^" ++ str ++ "\\s*=")
          then (return . Just . rep "^[^=]*=\\s*") ln
          else cfgLoop str hdl

-- Regex functions
rep :: String -> String -> String
rep pat = flip (subRegex (mkRegex pat)) ""

