{-# LANGUAGE OverloadedStrings #-}
module Main where
{-
-- terminal nix env
nix-shell --packages 'haskellPackages.ghcWithHoogle (pkgs: [ pkgs.text pkgs.safe ])' haskellPackages.ghcid
-- reloading while editing
ghcid --command='ghci align-equals.hs'
-}

import           Data.Text                      ( Text )
import qualified Data.Text
import qualified Data.Text.IO
import           Safe

prefixLength :: Text -> Int
prefixLength line = Data.Text.length prefix
  where (prefix, _) = Data.Text.breakOn "=" line

adjustLine :: Int -> Text -> Text
adjustLine desiredPrefixLength oldLine = newLine
  where (prefix, suffix) = Data.Text.breakOn "=" oldLine
        actualPrefixLength = Data.Text.length prefix
        additionalSpaces = desiredPrefixLength - actualPrefixLength
        spaces = Data.Text.replicate additionalSpaces " "
        newLine = Data.Text.concat [prefix, spaces, suffix]

adjustText :: Text -> Text
adjustText oldText = newText
  where oldLines = Data.Text.lines oldText
        prefixLengths = map prefixLength oldLines
        newLines = case Safe.maximumMay prefixLengths of
                     Nothing -> []
                     Just desiredPrefixLength -> map (adjustLine desiredPrefixLength) oldLines
        newText = Data.Text.unlines newLines

main :: IO ()
main = Data.Text.IO.interact adjustText












