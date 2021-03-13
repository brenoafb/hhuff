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
      printInfoT d
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
      printInfoB d
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


printInfoT :: EncodingDataT -> IO ()
printInfoT edt = do
  putStrLn $ "Encoded cotent size: " <> show (B.length $ contentsT edt) <> " bytes"
  putStrLn $ "Padding size: " <> (show $ paddingT edt)
  putStrLn $ "Average symbol length: " <> show (averageSymbolLength $ tableT edt) <> " bits"

printInfoB :: EncodingDataB -> IO ()
printInfoB edt = do
  putStrLn $ "Encoded cotent size: " <> show (B.length $ contentsB edt) <> " bytes"
  putStrLn $ "Padding size: " <> (show $ paddingB edt)
  putStrLn $ "Average symbol length: " <> show (averageSymbolLength $ tableB edt) <> " bits"

averageSymbolLength :: Table a -> Double
averageSymbolLength (Table m) = s / n
  where codes = M.elems m
        s = sum $ map (fromIntegral . length) codes
        n = fromIntegral $ length codes
