-- stack --install-ghc runghc --package rss --package filepath --package MissingH

import Data.List (sortOn, isPrefixOf, find)
import Data.Maybe (fromJust)
import qualified Data.Time
import qualified Network.URI
import qualified System.Environment

-- Provided by rss.
import qualified Text.RSS

-- Provided by filepath.
import qualified System.FilePath.Posix

-- Provided by MissingH.
import Data.String.Utils (strip)
import Data.List.Utils (split)
import Data.Maybe.Utils (forceMaybeMsg)

-- For more explanation of what fields mean, etc., see the RSS spec at http://www.rssboard.org/rss-specification

feedTitle = "reallyeli.com"
feedSiteUrl = "https://reallyeli.com"
feedDescription = "Eli Rose's blog"


data Post = Post {
  slug :: Slug,
  title :: String,
  date :: Data.Time.UTCTime, -- The beginning of the day on which the post was published.
  content :: Content, -- The markdown content of the post.
  isFinished :: Bool -- Is this post ready to be published?
}
type Slug = String
type Content = String


main :: IO ()
main = do
  postPaths <- System.Environment.getArgs
  posts <- getPosts postPaths
  putStrLn $ Text.RSS.showXML $ Text.RSS.rssToXML $ toRSSFeed posts


getPosts :: [String] -> IO [Post]
getPosts postPaths = do
  fileContents <- mapM readFile postPaths

  let slugs = map toSlug postPaths
  let posts = map toPost $ zip slugs fileContents
  let finishedPosts = filter isFinished posts
  
  return $ finishedPosts


-- e.g. "/home/steven/foo/bar.md" --> "bar"
toSlug :: String -> String
toSlug = System.FilePath.Posix.takeBaseName


toPost :: (Slug, Content) -> Post
toPost (slug, content) = let
  title :: String
  noTitleFound = "No title found for post with slug '" ++ slug ++ "'."
  title = forceMaybeMsg noTitleFound $ getKey "Title" content

  date :: Data.Time.UTCTime
  understandTime = Data.Time.parseTimeOrError True Data.Time.defaultTimeLocale "%Y-%m-%d"
  dateString = getKey "Date" content
  invalidDate = "Could not parse '" ++ (show dateString) ++ "' as date for post with slug '" ++ slug ++ "'."
  date = understandTime $ forceMaybeMsg invalidDate dateString

  isFinished :: Bool
  -- A post is considered "finished" if its status is not equal to "unfinished".
  isFinished = maybe True (/= "unfinished") (getKey "Status" content)

  in

  Post {slug=slug, title=title, date=date, content=content, isFinished=isFinished}


-- Metadata is stored in the posts as key/value pairs -- e.g. "Title: Boy" -- separated by newlines.
-- This function extracts the value for a given key (if it exists).
getKey :: String -> Content -> Maybe String
getKey key content = let
  lineWithKey :: Maybe String
  lineWithKey = find (isPrefixOf key) $ lines content

  in

  case lineWithKey of
    Nothing -> Nothing
    Just line -> Just (strip $ last $ split ":" line)


-- Creates an RSS feed object with the appropriate feed-level metadata -- title, link, description
-- and the posts as items.
toRSSFeed :: [Post] -> Text.RSS.RSS
toRSSFeed posts = let
  sortedPosts = reverse $ sortOn date $ posts

  in

  Text.RSS.RSS

  feedTitle

  -- Link to the website for the feed.
  -- This fromJust should never fail, as we know this is a valid URL.
  (fromJust $ Network.URI.parseURI feedSiteUrl)

  feedDescription

  -- Additional optional channel elements.
  [
    Text.RSS.Language "en"
  ]

  -- The feed items.
  (map toRSSItem sortedPosts)


-- Converts an individual Post to an RSS item.
toRSSItem :: Post -> Text.RSS.Item
toRSSItem Post{slug=slug, title=title, date=date, content=content} = let
  urlString = feedSiteUrl ++ "/posts/" ++ slug
  errorMessage = "Could not parse '" ++ urlString ++ "' as URI for post with slug '" ++ slug ++ "'."
  url = forceMaybeMsg errorMessage $ Network.URI.parseURI urlString

  in

  [
    Text.RSS.Title title,
    Text.RSS.Link url,
    Text.RSS.Description "", -- Intentionally left empty.
    Text.RSS.PubDate date,
    
    -- The boolean argument indicates that this is a permalink.
    -- http://www.rssboard.org/rss-specification#ltguidgtSubelementOfLtitemgt
    Text.RSS.Guid True (show url)
  ]
