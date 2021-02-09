module Util.File where

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, doesPathExist, removeDirectoryRecursive, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), replaceExtension, isExtensionOf)
import Text.Printf (printf)

prologExt :: FilePath -> FilePath
prologExt p = replaceExtension p "pl"

ocamlExt :: FilePath -> FilePath
ocamlExt p = replaceExtension p "ml"

checkIfFileExists :: Maybe String -> String -> IO FilePath
checkIfFileExists directory file = do
  let fileName = fromMaybe "" directory </> file
  exists <- doesFileExist fileName
  unless exists (fail $ printf "The file %s does not exist" fileName)
  return fileName

createDirRemoveExisting :: FilePath -> IO ()
createDirRemoveExisting path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)
  createDirectoryIfMissing True path

shortenFileName :: FilePath -> FilePath
shortenFileName =
  take 200 . filter (/=' ')

isDir :: FilePath -> IO Bool
isDir path = do
  d <- doesDirectoryExist path
  f <- doesFileExist path
  return $ d && not f

isFile :: FilePath -> IO Bool
isFile path = do
  d <- doesDirectoryExist path
  f <- doesFileExist path
  return $ f && not d

failIfNotExist :: FilePath -> IO ()
failIfNotExist path = do
  exist <- doesPathExist path
  unless exist (fail $ printf "%s does not exist" path)

failIfNotDir :: FilePath -> IO ()
failIfNotDir path = do
  d <- isDir path
  unless d (fail $ printf "%s is not a directory" path)

getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext dir = do
  files <- listDirectory dir
  return $ filter (isExtensionOf ext) files