{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : FlickrFilter
-- Description : Pandoc filter that rewrites Flickr photo-page links as figures
-- License     : MIT
-- Maintainer  : andrew@maier.name
--
-- This module provides 'transformBlocks', which walks a Pandoc block list and
-- rewrites any standalone Flickr photo-page link or image into a 'Figure'
-- block.  The static image URL is fetched from the Flickr oEmbed API at
-- transformation time.
--
-- Recognised input forms (all on their own paragraph):
--
-- * @!&#91;My caption&#93;(https://www.flickr.com/photos/alice/12345678/)@
-- * @&#91;Link text&#93;(https://www.flickr.com/photos/alice/12345678/)@
-- * A bare @Figure@ block whose single image points at a Flickr photo page
module FlickrFilter (transformBlocks) where

import           Control.Exception    (SomeException, try)
import           Control.Monad        (forM)
import           Data.Aeson           (FromJSON (..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Client.OpenSSL (newOpenSSLManager)
import           OpenSSL                     (withOpenSSL)
import           System.IO            (hPutStrLn, stderr)
import           Text.Pandoc.Definition

-- ---------------------------------------------------------------------------
-- oEmbed
-- ---------------------------------------------------------------------------

-- | Minimal representation of a Flickr oEmbed JSON response.
-- We only need the @url@ field (the static image URL).
newtype OEmbedResponse = OEmbedResponse
  { oEmbedUrl :: T.Text
  } deriving (Show, Generic)

instance FromJSON OEmbedResponse where
  parseJSON = withObject "OEmbedResponse" $ \ v ->
    OEmbedResponse <$> v .: "url"

-- | Query the Flickr oEmbed API for @flickrUrl@ and return the static image
-- URL, or 'Nothing' if the request fails or the response cannot be parsed.
-- Errors are logged to 'stderr' and do not propagate as exceptions.
fetchStaticUrl :: T.Text -> IO (Maybe T.Text)
fetchStaticUrl flickrUrl = do
  let endpoint =
        "https://www.flickr.com/services/oembed/?url="
          <> T.unpack flickrUrl
          <> "&format=json&maxwidth=2048"
  result <- try $ withOpenSSL $ do
    manager  <- newOpenSSLManager
    request  <- parseRequest endpoint
    response <- httpLbs request manager
    return $ fmap oEmbedUrl $ decode (responseBody response)
  case result of
    Left err -> do
      hPutStrLn stderr $
        "flickr-filter: could not fetch oEmbed for "
          <> T.unpack flickrUrl
          <> ": "
          <> show (err :: SomeException)
      return Nothing
    Right mUrl -> return mUrl

-- ---------------------------------------------------------------------------
-- AST transformation
-- ---------------------------------------------------------------------------

-- | Return 'True' for URLs that point at a Flickr photo page
-- (@\/photos\/<user>\/<id>\/@).
isFlickrPhotoUrl :: T.Text -> Bool
isFlickrPhotoUrl u =
  "https://www.flickr.com/photos/" `T.isPrefixOf` u ||
  "http://www.flickr.com/photos/"  `T.isPrefixOf` u

-- | Return 'True' for 'Space' and 'SoftBreak' inlines, which are ignored
-- when deciding whether a paragraph contains a single Flickr reference.
isWhitespaceInline :: Inline -> Bool
isWhitespaceInline Space     = True
isWhitespaceInline SoftBreak = True
isWhitespaceInline _         = False

-- | A recognised Flickr reference extracted from a paragraph or figure.
data FlickrRef = FlickrRef
  { frCaption :: [Inline] -- ^ Caption/alt-text inlines
  , frPageUrl :: T.Text   -- ^ Flickr photo-page URL
  , frTitle   :: T.Text   -- ^ Link title attribute (may be empty)
  }

-- | Try to extract a 'FlickrRef' from the inlines of a paragraph.
-- Returns 'Nothing' if the paragraph contains anything other than a single
-- Flickr image or link (whitespace inlines are ignored).
detectFlickrRef :: [Inline] -> Maybe FlickrRef
detectFlickrRef inlines =
  case filter (not . isWhitespaceInline) inlines of
    [Image _ caption (u, title)]
      | isFlickrPhotoUrl u ->
          Just (FlickrRef caption u title)
    [Link _ content (u, title)]
      | isFlickrPhotoUrl u
      , not (any (\i -> case i of { Image {} -> True; _ -> False }) content) ->
          Just (FlickrRef content u title)
    _ -> Nothing

-- | Build the replacement 'Figure' block given a 'FlickrRef' and the
-- resolved static image URL.
buildReplacement :: FlickrRef -> T.Text -> [Block]
buildReplacement ref staticUrl =
  [ Figure nullAttr
      (Caption Nothing [Plain (frCaption ref)])
      [Plain [Link nullAttr [Image imgAttr [] (staticUrl, "")] (frPageUrl ref, frTitle ref)]]
  ]
  where
    imgAttr = ("", [], [("width", "80%")])

-- | Extract the inline content of a 'Plain' or 'Para' block, returning @[]@
-- for all other block types.
blockInlines :: Block -> [Inline]
blockInlines (Plain ils) = ils
blockInlines (Para  ils) = ils
blockInlines _           = []

-- | Recursively transform a list of 'Block' elements, replacing any
-- standalone Flickr photo-page link or image with a 'Figure' block that
-- contains the static image (fetched via the oEmbed API) and a caption.
--
-- Blocks that do not match are returned unchanged.  Container blocks
-- ('Div', 'BlockQuote', lists, etc.) are descended into.
transformBlocks :: [Block] -> IO [Block]
transformBlocks = fmap concat . mapM transformBlock

-- | Transform a single 'Block', returning a list so that a match can be
-- replaced by exactly one 'Figure' without changing the surrounding structure.
transformBlock :: Block -> IO [Block]
transformBlock (Para inlines)
  | Just ref <- detectFlickrRef inlines = do
      mStaticUrl <- fetchStaticUrl (frPageUrl ref)
      case mStaticUrl of
        Just staticUrl -> return $ buildReplacement ref staticUrl
        Nothing        -> return [Para inlines]
transformBlock (Figure _ (Caption _ captionBlocks) [Plain [Image _ _ (u, title)]])
  | isFlickrPhotoUrl u = do
      let captionInlines = concatMap blockInlines captionBlocks
      mStaticUrl <- fetchStaticUrl u
      case mStaticUrl of
        Just staticUrl -> return $ buildReplacement (FlickrRef captionInlines u title) staticUrl
        Nothing        -> return [Figure nullAttr (Caption Nothing captionBlocks) [Plain [Image nullAttr [] (u, title)]]]
transformBlock (Div attr bs) = do
  bs' <- transformBlocks bs
  return [Div attr bs']
transformBlock (BlockQuote bs) = do
  bs' <- transformBlocks bs
  return [BlockQuote bs']
transformBlock (BulletList items) = do
  items' <- mapM transformBlocks items
  return [BulletList items']
transformBlock (OrderedList la items) = do
  items' <- mapM transformBlocks items
  return [OrderedList la items']
transformBlock (DefinitionList items) = do
  items' <- forM items $ \(term, defs) -> do
    defs' <- mapM transformBlocks defs
    return (term, defs')
  return [DefinitionList items']
transformBlock b = return [b]
