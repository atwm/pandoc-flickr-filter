{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Pandoc JSON filter executable for pandoc-flickr-filter
-- License     : MIT
-- Maintainer  : andrew@maier.name
--
-- Reads a Pandoc JSON AST from @stdin@, applies 'FlickrFilter.transformBlocks',
-- and writes the result to @stdout@.  Intended to be used as:
--
-- > pandoc --filter pandoc-flickr-filter input.md -o output.html
module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import           FlickrFilter         (transformBlocks)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)
import           Text.Pandoc.Definition

main :: IO ()
main = do
  contents <- BL.getContents
  case Aeson.decode contents of
    Nothing -> do
      hPutStrLn stderr "flickr-filter: failed to parse Pandoc JSON from stdin"
      exitFailure
    Just (Pandoc meta blocks) -> do
      blocks' <- transformBlocks blocks
      BL.putStr (Aeson.encode (Pandoc meta blocks'))
