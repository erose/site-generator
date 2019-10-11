-- stack --install-ghc runghc --package rss --package filepath --package tagsoup --package cmark --package MissingH
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortOn, isPrefixOf, find, take)
import Data.Maybe (fromJust)
import qualified Data.Text
import qualified Data.Time
import qualified Network.URI
import qualified System.Environment

-- Provided by rss.
import qualified Text.RSS

-- Provided by filepath.
import qualified System.FilePath.Posix

-- Provided by tagsoup.
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.HTML.TagSoup.Tree as TagSoup.Tree

-- Provided by cmark.
import qualified CMark

-- Provided by MissingH.
import Data.String.Utils (strip)
import Data.List.Utils (split, join)
import Data.Maybe.Utils (forceMaybeMsg)

-- For more explanation of what fields mean, etc., see the RSS spec at http://www.rssboard.org/rss-specification

feedTitle = "reallyeli.com"
feedSiteUrl = "https://reallyeli.com"
feedDescription = "Eli Rose's blog"


data Post = Post {
  slug :: Slug,
  title :: String,
  date :: Data.Time.UTCTime, -- The beginning of the day on which the post was published.
  content :: HTMLContent, -- The HTML content of the post.
  isFinished :: Bool -- Is this post ready to be published?
}
type Slug = String
type RawPostContent = String
type HTMLContent = Data.Text.Text


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


toPost :: (Slug, RawPostContent) -> Post
toPost (slug, rawPostContent) = let

  title :: String
  noTitleFound = "No title found for post with slug '" ++ slug ++ "'."
  title = forceMaybeMsg noTitleFound $ getKey "Title" rawPostContent

  date :: Data.Time.UTCTime
  understandTime = Data.Time.parseTimeOrError True Data.Time.defaultTimeLocale "%Y-%m-%d"
  dateString = getKey "Date" rawPostContent
  invalidDate = "Could not parse '" ++ (show dateString) ++ "' as date for post with slug '" ++ slug ++ "'."
  date = understandTime $ forceMaybeMsg invalidDate dateString

  isFinished :: Bool
  -- A post is considered "finished" if its status is not equal to "unfinished".
  isFinished = maybe True (/= "unfinished") (getKey "Status" rawPostContent)

  htmlContent :: HTMLContent
  htmlContent = CMark.commonmarkToHtml [] $ Data.Text.pack (withoutKeys rawPostContent)

  in

  Post {slug=slug, title=title, date=date, content=htmlContent, isFinished=isFinished}


-- Metadata is stored in the posts as key/value pairs -- e.g. "Title: Boy" -- separated by newlines.
-- This function extracts the value for a given key (if it exists).
getKey :: String -> RawPostContent -> Maybe String
getKey key rawPostContent = let
  lineWithKey :: Maybe String
  lineWithKey = find (isPrefixOf key) $ lines rawPostContent

  in

  case lineWithKey of
    Nothing -> Nothing
    Just line -> Just (strip $ last $ split ":" line)


-- Metadata is stored in the posts as key/value pairs -- e.g. "Title: Boy" -- separated by newlines.
-- This function strips the metadata from a post.
withoutKeys :: RawPostContent -> RawPostContent
withoutKeys rawPostContent = let
  -- A single blank line separates the keys and the content.
  withoutKeys = dropWhile (/= "") (lines rawPostContent)
  withoutKeysAndNextBlankLine = drop 1 withoutKeys

  in

  join "\n" $ withoutKeysAndNextBlankLine


-- We need to find a shorter version of the content of a post to display in the feed.
truncateContentForPreview :: HTMLContent -> HTMLContent
truncateContentForPreview content = let
  trees = TagSoup.Tree.parseTree content

  treeIsAllowed :: TagSoup.Tree.TagTree HTMLContent -> Bool
  treeIsAllowed tree = 
    case tree of
      TagSoup.Tree.TagBranch "p" _ _ -> True

      -- It looks like TagSoup parses '<img src="...">' as TagBranch and '<img src="..."/>' as a
      -- TagLeaf containing a single TagOpen.
      TagSoup.Tree.TagBranch "img" _ _ -> True
      TagSoup.Tree.TagLeaf (TagSoup.TagOpen "img" _) -> True
      otherwise -> False

  -- Our strategy is to grab the trees representing one of a whitelisted set of tags, and show the
  -- first one.
  allowedTrees = filter treeIsAllowed $ TagSoup.Tree.universeTree trees

  renderOptions = TagSoup.renderOptions { TagSoup.optRawTag = (\_ -> True) }
  
  in

  TagSoup.Tree.renderTreeOptions renderOptions $ take 1 allowedTrees


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

  description :: HTMLContent
  description = truncateContentForPreview $ withQualifiedUrls content

  in

  [
    Text.RSS.Title title,
    Text.RSS.Link url,
    Text.RSS.Description (Data.Text.unpack description),
    Text.RSS.PubDate date,
    
    -- The boolean argument indicates that this is a permalink.
    -- http://www.rssboard.org/rss-specification#ltguidgtSubelementOfLtitemgt
    Text.RSS.Guid True (show url)
  ]

-- src attributes for <img> tags need to have fully qualified URLs in order to appear properly when
-- viewing the description HTML not on the site, for example in a reader or in an email.
withQualifiedUrls :: HTMLContent -> HTMLContent
withQualifiedUrls html = let
  singleQuotesDealtWith = Data.Text.replace "src=\"/" (Data.Text.pack $ "src=\"" ++ feedSiteUrl ++ "/") html
  singleAndDoubleQuotesDealtWith = Data.Text.replace "src='" (Data.Text.pack $ "src='" ++ feedSiteUrl ++ "/") singleQuotesDealtWith

  in

  singleAndDoubleQuotesDealtWith
