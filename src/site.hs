--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.List (partition)
import Data.Monoid (mappend, (<>))
import System.Environment (getArgs, withArgs)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = checkArgs <$> getArgs >>=
        \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["pages/about.md", "pages/404.md"]) $ do
        route   $ stripPages `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route stripPages
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" (postCtx tags) (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match postsPattern $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/post-with-comment.html" defaultContext
            >>= loadAndApplyTemplate "templates/right-column.html" (mainCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx tags) `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postsPattern "content"
            renderAtom feedCfg feedCtx posts

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "pages/index.html" $ do
        route stripPages
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots postsPattern "content"
            let indexCtx =
                    listField "posts" (previewCtx tags) (return posts) `mappend`
                    mainCtx tags

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/right-column.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
stripPages = gsubRoute "pages/" $ const ""

mainCtx :: Tags -> Context String
mainCtx tags =
    let recentPosts = postItems >>= fmap (take 5) . recentFirst in
      listField "recentPosts" (previewCtx tags) recentPosts `mappend`
      tagCloudField "tagCloud" 75 200 tags `mappend`
      defaultContext

postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" `mappend`
    tagsField "tags" tags `mappend`
    defaultContext

previewCtx :: Tags -> Context String
previewCtx tags = teaserField "preview" "content" `mappend` postCtx tags

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
    { feedTitle = "Mike Limansky blog"
    , feedDescription = "Latest blog posts"
    , feedAuthorName = "Mike Limansky"
    , feedAuthorEmail = "mike.limansky@gmail.com"
    , feedRoot = "http://www.limansky.me"
    }

-- Check argrumens for '--with-drafts'
-- returns post pattern, configuration, command arguments
checkArgs :: [String] -> (Pattern, Configuration, [String])
checkArgs args = case partition (/= "--with-drafts") args of
    (_, []) -> ("posts/*",                  defaultConfiguration,   args)
    (as, _) -> ("posts/*" .||. "drafts/*",  draftConf,              as)
    where draftConf = defaultConfiguration {
        destinationDirectory = "_draftSite"
      , storeDirectory = "_draftCache"
      , tmpDirectory = "_draftCache/tmp"
      }

postItems :: Compiler [Item String]
postItems = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]
