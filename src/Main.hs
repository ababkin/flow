{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
{- import Data.Aeson (decode, FromJSON(parseJSON), (.:), Value(Object)) -}
{- import Data.Aeson.Types (typeMismatch) -}
import System.Process (readProcess)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.IO.Strict as SIO
import Text.ParserCombinators.Parsec

data Branch = LocalBranch{
    bName :: String
  } | RemoteBranch{
    bName       :: String
  , originName  :: String
  } deriving (Show)


main :: IO ()
main = do

  originalBranch <- currentBranch
  branch <- createBranch $ LocalBranch "temp_branch"

  branches <- listAllBranches
  putStr $ "all branches: " ++ (show branches)

  checkout originalBranch
  destroyBranch branch

    where

      currentBranch = (head . parsedBranches) <$> git ["rev-parse", "--abbrev-ref", "HEAD"]
      
      createBranch branch@(LocalBranch name) = do
        git ["checkout", "-b", name]
        return branch

      destroyBranch (LocalBranch name) = do
        git ["branch", "-D", name]
        return ()

      listAllBranches :: IO ([Branch])
      listAllBranches = parsedBranches <$> git ["branch", "-a"]

      checkout (LocalBranch name) = git ["checkout", name]

      git args = readProcess "git" args ""


      parsedBranches :: String -> [Branch]
      parsedBranches s = case parse parseBranches "" s of
        Right branches -> branches
        Left err -> []

{- parseTs = many1 $ parseT -}

{- parseT = do -}
  {- t <- manyTill (noneOf "\n") $ eol -}
  {- return t -}

        where
          parseBranches = many1 $ (try activeLocalBranch)
                                    <|> (try remoteBranch)
                                    <|> localBranch

          activeLocalBranch = do
            char '*'
            spaces
            branchName <- manyTill (noneOf "\n") $ eol
            return $ LocalBranch branchName

          remoteBranch = do
            spaces
            string "remotes/"
            originName <- manyTill (noneOf "/") $ char '/'
            branchName <- manyTill (noneOf "\n") $ eol
            return $ RemoteBranch branchName originName

          localBranch = do
            spaces
            branchName <- manyTill (noneOf "\n") $ eol
            return $ LocalBranch branchName

          eol = char '\n'



