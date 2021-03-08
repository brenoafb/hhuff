{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.Binary as Bin
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Code
import Tree

main :: IO ()
main = getArgs >>= parse

parse ["-b", inputFile, outputFile] = binaryEncode inputFile outputFile
parse ["-B", inputFile, outputFile] = binaryDecode inputFile outputFile
parse ["-t", inputFile, outputFile] = textEncode inputFile outputFile
parse ["-T", inputFile, outputFile] = textDecode inputFile outputFile
parse _ = putStr usage

usage = unlines
  [ "usage: -[t/b/T/B] input-file output-file"
  , "-t: encode in text mode"
  , "-T: decode in text mode"
  , "-b: encode in binary mode"
  , "-B: decode in binary mode"
  ]

textEncode inputFile outputFile = do
  input <- TIO.readFile inputFile
  case encodeT input of
    Nothing -> putStrLn "Encoding error"
    Just d -> do
      putStrLn $ "Encoded content size: " <> show (B.length $ contentsT d) <> " bytes"
      Bin.encodeFile outputFile d

textDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d -> do
      let decodedM = decodeT d
      case decodedM of
        Left err -> print err
        Right text -> do
          TIO.writeFile outputFile text

binaryEncode inputFile outputFile = do
  input <- B.readFile inputFile
  case encodeB input of
    Nothing -> putStrLn "Encoding error"
    Just d -> do
      putStrLn $ "Encoded content size: " <> show (B.length $ contentsB d) <> " bytes"
      Bin.encodeFile outputFile d

binaryDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d -> do
      let decodedM = decodeB d
      case decodedM of
        Left err -> print err
        Right bytes -> do
          B.writeFile outputFile bytes

printTable (Table table) =
  mapM_ (\(x, y) -> putStrLn (show x ++ "\t" ++ show y))
        $ M.toList table
