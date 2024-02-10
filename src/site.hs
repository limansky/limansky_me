--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Control.Applicative ((<$>))
import Data.List (partition)
import qualified Data.Map as M
import Data.Monoid ((<>))
import System.Environment (getArgs, withArgs)
import qualified System.Process as Process
import System.Exit (ExitCode)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = checkArgs <$> getArgs >>=
        \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["pages/about.md", "pages/404.md"]) $ do
        route   $ stripPages `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" (postCtx tags) (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match postsPattern $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post-with-comment.html" defaultContext
            >>= loadAndApplyTemplate "templates/post-right-column.html" (postCtx tags <> mainCtx tags postsPattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postsPattern "content"
            renderAtom feedCfg feedCtx posts

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            singlePages <- loadAll (fromList ["pages/about.md" ])
            let pages = posts <> singlePages
                sitemapCtx =
                    constField "root" root <> -- here
                    dateField "date" "%Y-%m-%d" <>
                    listField "pages" sitemapCtx (return pages) <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    paginate <- buildPaginateWith postsGrouper postsPattern postsPageId

    paginateRules paginate $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let indexCtx =
                    constField "title" (if page == 1 then "Latest blog posts"
                                                     else "Blog posts, page " ++ show page) <>
                    listField "posts" (previewCtx tags) (return posts) <>
                    paginateContextPlus paginate page <>
                    mainCtx tags postsPattern

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/posts-preview-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/page-right-column.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
stripPages = gsubRoute "pages/" $ const ""

root :: String
root = "http://limansky.me"

mainCtx :: Tags -> Pattern -> Context String
mainCtx tags postsPattern =
    let recentPosts = postItems postsPattern >>= fmap (take 5) . recentFirst in
      listField "recentPosts" (previewCtx tags) recentPosts <>
      tagCloudField "tagCloud" 75 200 tags <>
      defaultContext

postCtx :: Tags -> Context String
postCtx tags =
    constField "root" root <>
    dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags <>
    defaultContext

previewCtx :: Tags -> Context String
previewCtx tags = teaserField "preview" "content" <> postCtx tags

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
    (_, []) -> ("posts/*",                  config,                 args)
    (as, _) -> ("posts/*" .||. "drafts/*",  draftConf,              as)
    where draftConf = config {
        destinationDirectory = "_draftSite"
      , storeDirectory = "_draftCache"
      , tmpDirectory = "_draftCache/tmp"
      }

config :: Configuration
config = defaultConfiguration {
      deploySite = deploy
    }
  where
    deploy :: Configuration -> IO ExitCode
    deploy _ = do
      branch <- Process.readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
      case words branch of
        ["master"] -> Process.rawSystem "rsync" [ "--checksum", "-av", "_site/", "limansky.me:/var/www/limansky"]

postItems :: Pattern ->  Compiler [Item String]
postItems postsPattern = do
    identifiers <- getMatches postsPattern
    return [Item identifier "" | identifier <- identifiers]

postsGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
postsGrouper = fmap (paginateEvery 10) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if n == 1 then "index.html" else show n ++ "/index.html"

paginateContextPlus :: Paginate -> PageNumber -> Context a
paginateContextPlus pag currentPage = paginateContext pag currentPage <> mconcat
    [ listField "pagesBefore" linkCtx $ wrapPages pagesBefore
    , listField "pagesAfter"  linkCtx $ wrapPages pagesAfter
    ]
    where
        linkCtx = field "pageNum" (return . fst . itemBody) <>
                  field "pageUrl" (return . snd . itemBody)
        lastPage = M.size . paginateMap $ pag
        pageInfo n = (n, paginateMakeId pag n)

        pages = [pageInfo n | n <- [1..lastPage], n /= currentPage]
        (pagesBefore, pagesAfter) = span ((< currentPage) . fst) pages

        wrapPages = mapM makeInfoItem

        makeInfoItem (n, i) = getRoute i >>= \case
            Just r  -> makeItem (show n, toUrl r)
            Nothing -> fail $ "No URL for page: " ++ show n
