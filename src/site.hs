--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           System.Environment (getArgs, withArgs)
import           Control.Applicative ((<$>))

--------------------------------------------------------------------------------
main :: IO ()
main = checkArgs <$> getArgs >>= \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do
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
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
stripPages = gsubRoute "pages/" $ const ""

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
checkArgs args = if (elem "--with-drafts" args)
    then ("posts/*" .||. "drafts/*", draftConf, filter (/= "--with-drafts") args)
    else ("posts/*", defaultConfiguration, args)
    where draftConf = defaultConfiguration {
        destinationDirectory = "_draftSite"
      , storeDirectory = "_draftCache"
      , tmpDirectory = "_draftCache/tmp"
      }
