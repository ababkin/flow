{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
{- import Data.Aeson (decode, FromJSON(parseJSON), (.:), Value(Object)) -}
{- import Data.Aeson.Types (typeMismatch) -}
{- import Text.Regex.Posix -}
import System.Process (readProcess)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.IO.Strict as SIO

main :: IO ()
main = do
  createTempBranch
  result <- readProcess command [] ""
  putStr $ "result: " ++ result
    where
      command = "ls"

      createTempBranch = readProcess "git" ["checkout", "-b temp_branch"] ""
      destroyTempBranch = readProcess "git" ["branch", "-D temp_branch"] ""
        

