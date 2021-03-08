{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Code where

import Bits
import Tree

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State

import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Maybe (fromJust)
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T

newtype Table a = Table (M.Map a BitString)
  deriving Generic

instance Bin.Binary a => Bin.Binary (Table a)

type Error = T.Text

data EncodingDataT =
     EncodingDataT { tableT :: Table Char
                   , paddingT :: Int
                   , contentsT :: B.ByteString
                   } deriving (Generic)

instance Bin.Binary EncodingDataT

data EncodingDataB =
     EncodingDataB { tableB :: Table Word8
                   , paddingB :: Int
                   , contentsB :: B.ByteString
                   } deriving (Generic)

instance Bin.Binary EncodingDataB

encodeT input = do
  (table, padding, bs) <- encode $ T.unpack input
  pure $ EncodingDataT { tableT    = table
                       , paddingT  = padding
                       , contentsT = bs
                       }

encodeB input = do
  (table, padding, bs) <- encode $ B.unpack input
  pure $ EncodingDataB { tableB    = table
                       , paddingB  = padding
                       , contentsB = bs
                       }

decodeT (EncodingDataT table padding contents) = do
  let contents' = drop padding $ fromByteString contents
  T.pack <$> decode contents' table

decodeB (EncodingDataB table padding contents) = do
  let contents' = drop padding $ fromByteString contents
  B.pack <$> decode contents' table

encode :: (Traversable t, Ord a) => t a -> Maybe (Table a, Int, B.ByteString)
encode input = do
  let f = freqs input
  tree <- buildTree f
  table@(Table t) <- mkTable tree
  encoded <- concat <$> traverse (`M.lookup` t) input
  let (bs, padding) = toByteString encoded
  pure (table, padding, bs)

-- decode :: Ord a => BitString -> Table a -> Maybe [a]
decode :: Ord a => BitString -> Table a -> Either Error [a]
decode bits (Table t) = runIdentity $ runExceptT $ runReaderT (evalStateT s bits) (t', k)
  where s = try `untilM` gets null
        t' = reverseMap t
        k  = M.keys t'
        try :: StateT BitString (ReaderT (M.Map BitString a, [BitString]) (ExceptT Error Identity)) a
        try = do
          bits <- get
          (m, ks) <- ask
          case L.find (`L.isPrefixOf` bits) ks of
            Nothing -> throwError "No valid prefix"
            Just k  -> do
              let n = length k
                  bits' = drop n bits
              put bits'
              pure . fromJust $ M.lookup k m -- k is guaranteed to be in m

getCode :: (Eq a) => Tree a n -> a -> Maybe BitString
getCode (Leaf _ c) x
  | c == x = pure []
  | otherwise = Nothing
getCode (Node _ xs t1 t2) x
  | x `elem` xs = ((O :) <$> getCode t1 x) <|> ((I :) <$> getCode t2 x)
  | otherwise   = Nothing

mkTable :: Ord a => Tree a n -> Maybe (Table a)
mkTable (Leaf _ x) = pure . Table $ M.fromList [(x, [])]
mkTable t@(Node _ xs _ _) = do
  codes <- traverse (\x -> sequenceA (x, getCode t x)) xs
  pure . Table $ M.fromList codes

buildTree :: M.Map a Int -> Maybe (Tree a Int)
buildTree = go . map (\(x, w) -> Leaf w x) . M.toList
  where go :: (Num n, Ord n) => [Tree a n] -> Maybe (Tree a n)
        go [] = Nothing
        go [t] = pure t
        go ls = do
          let (x1:x2:xs) = L.sortOn getTreeWeight ls
              merged = merge x1 x2 : xs
          go merged

freqs :: (Ord a, Traversable t) => t a -> M.Map a Int
freqs xs = execState s M.empty
  where s = traverse updateCount xs
        updateCount :: (Ord a, MonadState (M.Map a Int) m) => a -> m ()
        updateCount x = do
          m <- get
          case M.lookup x m of
            Nothing -> modify (M.insert x 1)
            Just n  -> modify (M.insert x $ n + 1)

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map (\(x,y) -> (y,x)) . M.toList
