{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)
import Data.Foldable (traverse_)
import           Control.Concurrent.Async as Async
import           Control.Exception (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Search as Search
import           Data.List (nub, isInfixOf)
import           Data.Monoid ((<>))
import           Data.Traversable
import           GHC.Conc (threadDelay)
import           Network.HTTP.Simple
-- import qualified STMContainers.Map as Map

-- within 3 months


-- "application/atom+xml"
-- "application/rss+xml"

data FeedConfig
  = FeedConfig
  { name :: String
  , location :: String
  , dateFmt :: String
  } deriving (Show, Eq)

wordpressConfig :: String -> FeedConfig
wordpressConfig site = FeedConfig "wordpress"  (site ++ "/feed/")  "Wed, 23 Aug 2017 20"

blogspotConfig :: String -> FeedConfig
blogspotConfig site = FeedConfig "blogspot" ("http://feeds.feedburner.com/" ++ extractBlogspotName site ++ "?format=xml")  "Wed, 23 Aug 2017 20:10:10 +0100"

weeblyConfig :: String -> FeedConfig
weeblyConfig site =  FeedConfig "wordpress"  (site ++ "/1/feed/")  "Wed, 23 Aug 2017 20:10:10 +0100"

tumblrConfig site = undefined

extractBlogspotName s = undefined

attemptStrings :: [String]
attemptStrings = [ "/feed/" , "/feed.atom", "/feed.rss"] -- "/feed/atom/"]

mkAttemptStrings :: String -> [String]
mkAttemptStrings x = map (\a -> validate x a) attemptStrings
  where
    validate site ext =
      if "http" `isInfixOf` site
      then site ++ ext
      else "http://" ++ site ++ ext

isInfixBS :: BS.ByteString -> [BS.ByteString] -> Bool
isInfixBS pat target = any (not . null) $ map (Search.indices pat) target

siteResponse :: String -> IO (Either String SitePair)
siteResponse site = do
  r <- try $ httpLbs =<< parseRequest site
  case r of
    Left (_ :: HttpException) -> do
      -- log $ "failed to reach " ++ site
      pure $ Left site
    Right res -> do
      let body = getResponseBody res
      let content = getResponseHeader "Content-Type" res
      -- lol, I'm not parsing these xmls
      if ("xml" `isInfixBS` content)
        then pure $ Right (C8.pack site, LBS.toStrict body)
        else pure $ Left site

searchDates :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
searchDates term res =
  map (BS.drop 1 . BS.take 20) $ Search.split term res

getDateAttempts :: String -> Concurrently [Either String SitePair]
getDateAttempts site = Async.Concurrently $ do
  let attempts = mkAttemptStrings site
  results <- forM attempts $ \s -> do
    sr <- siteResponse s
    case sr of
      Left _ -> do
        -- log $ "NOT FOUND - " ++ site
        pure (Left site)
      Right (s',res) -> do
        let matches = searchDates "updated" res
        let matches2 = searchDates "pubDate" res
        let g = (s', maybeSecond $ nub $ matches ++ matches2)
        C8.putStrLn $ C8.unlines $ glue [g]
        return $ Right g
  return results

main :: IO ()
main = do
  sites <- readCsv

  Async.runConcurrently (traverse_ getDateAttempts sites)
  return ()


type SitePair = (BS.ByteString, BS.ByteString)

glue :: [SitePair] -> [BS.ByteString]
glue = map (\(s,d) -> s <> "\t" <> d)

maybeSecond (_:y:_) = y
maybeSecond _ = ""

readCsv :: IO [String]
readCsv = do
  sites <- lines <$> readFile "justsites.txt"
  return sites

mapSlow f = mapM (\a -> threadDelay 250 >> f a)
mapSlow_ f = mapM_ (\a -> threadDelay 250 >> f a)

log = putStrLn
