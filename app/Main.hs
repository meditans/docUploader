{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, run) where

import Lib
{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns #-}

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Functor
import Data.Tuple.Extra
import Data.Time
import System.Directory.Extra
import System.IO.Extra
import System.FilePath
import System.Process.Extra
import Prelude

defAllow = ["7.4.2","7.6.3","7.8.4","7.10.3","8.0.1"]

main = undefined

---------------------------------------------------------------------
-- COMMANDS

run :: Docs -> Maybe (IO ())
run Docs{..} = Just $ do
    src <- readCabal
    let ver = extractCabal "version" src
    let name = extractCabal "name" src
    system_ $ "cabal haddock --hoogle --html --hyperlink-source " ++
          "--contents-location=/package/" ++ name
    withTempDir $ \dir -> do
        system_ $ "cp -R dist/doc/html/" ++ name ++ " \"" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs\""
        files <- listFilesRecursive dir
        forM_ files $ \file -> when (takeExtension file == ".html") $ do
            system_ $ "chmod u+w " ++ (dir </> file)
            src <- readFileBinary' $ dir </> file
            src <- return $ filter (/= '\r') src -- filter out \r, due to CPP bugs
            src <- return $ fixFileLinks $ fixHashT src
            writeFileBinary (dir </> file) src
        system_ $ "tar cvz -C " ++ dir ++ " --format=ustar -f " ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz " ++ name ++ "-" ++ ver ++ "-docs"
        system_ $ "curl -X PUT -H \"Content-Type: application/x-tar\" " ++
              "-H \"Content-Encoding: gzip\" " ++
              "-u " ++ username ++ " " ++
              "--data-binary \"@" ++ dir ++ "/" ++ name ++ "-" ++ ver ++ "-docs.tar.gz\" " ++
              host ++ "/package/" ++ name ++ "-" ++ ver ++ "/docs"

fixHashT :: String -> String
fixHashT (stripPrefix ".html#t:" -> Just (x:xs)) | not $ isUpper x = ".html#v:" ++ fixHashT (x:xs)
fixHashT (x:xs) = x : fixHashT xs
fixHashT [] = []

fixFileLinks :: String -> String
fixFileLinks (stripPrefix "<a href=\"file://" -> Just xs)
    | (a,'\"':b) <- break (== '\"') xs
    , modu <- takeFileName a
    , pkg <- dropEnd 1 $ dropWhileEnd (/= '-') $ takeFileName $ dropHTML $ takeDirectory a
    = "<a href=\"/package/" ++ pkg ++ "/docs/" ++ modu ++ "\"" ++ fixFileLinks b
    where dropHTML x = if takeFileName x == "html" then takeDirectory x else x
fixFileLinks xs@(stripPrefix "<a href=\"file://" -> Just _) = error $ "Unable to remove file link, " ++ take 200 xs
fixFileLinks (x:xs) = x : fixFileLinks xs
fixFileLinks [] = []


readCabal :: IO String
readCabal = do
    file <- findCabal
    case file of
        Nothing -> return []
        Just file -> readFile' file

extractCabal :: String -> String -> String
extractCabal find = f . words . replace ":" " : "
    where
        f (name:":":val:_) | lower find == lower name = val
        f (x:xs) = f xs
        f [] = error "Failed to find the Cabal key " ++ find


findCabal :: IO (Maybe FilePath)
findCabal = do
    x <- getDirectoryContents "."
    return $ listToMaybe $ filter ((==) ".cabal" . takeExtension) x

data Docs = Docs {username :: String, host :: String}

