{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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

newtype OEmbedResponse = OEmbedResponse
  { oEmbedUrl :: T.Text
  } deriving (Show, Generic)

instance FromJSON OEmbedResponse where
  parseJSON = withObject "OEmbedResponse" $ \v ->
    OEmbedResponse <$> v .: "url"

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

isFlickrPhotoUrl :: T.Text -> Bool
isFlickrPhotoUrl u =
  "https://www.flickr.com/photos/" `T.isPrefixOf` u ||
  "http://www.flickr.com/photos/"  `T.isPrefixOf` u

isWhitespaceInline :: Inline -> Bool
isWhitespaceInline Space     = True
isWhitespaceInline SoftBreak = True
isWhitespaceInline _         = False

data FlickrRef = FlickrRef
  { frCaption :: [Inline]
  , frPageUrl :: T.Text
  , frTitle   :: T.Text
  }

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

buildReplacement :: FlickrRef -> T.Text -> [Block]
buildReplacement ref staticUrl =
  [ Figure nullAttr
      (Caption Nothing [Plain (frCaption ref)])
      [Plain [Link nullAttr [Image imgAttr [] (staticUrl, "")] (frPageUrl ref, frTitle ref)]]
  ]
  where
    imgAttr = ("", [], [("width", "80%")])

blockInlines :: Block -> [Inline]
blockInlines (Plain ils) = ils
blockInlines (Para  ils) = ils
blockInlines _           = []

-- | Recursively transform a list of Blocks, replacing standalone Flickr
-- links/images with a Figure containing the static image and a caption.
-- Safe to call multiple times (idempotent).
transformBlocks :: [Block] -> IO [Block]
transformBlocks = fmap concat . mapM transformBlock

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
