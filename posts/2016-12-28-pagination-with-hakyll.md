---
title: Pagination with Hakyll
tags: Hakyll, Haskell
---

Yesterday I realized that there are more than ten posts in my blog. Not so many
for more than a year, but quite a lot for the index page.  The simplest solution
is to just limit amount of posts with `take` function, and write something like
"more posts in the [archives](/archive.html)".  But I prefer to have pagination
for the index page.  Hakyll has built-in support for pagination in the module
[Hakyll.Web.Paginate](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Paginate.html).
There is a very nice manual about how to use it in this [blog
post](https://dannysu.com/2015/10/29/hakyll-pagination/).

Unfortunately, Paginate provides only first/previous/current/next/last functionality.
It's quite common to have only "Older posts" and "Newer posts" buttons for
blogs, but I'd like to have a list of all pages in the Bootstrap pagination
component.  So, it's time to write some Haskell code (of course you need to
write code to add the standard Paginate, but I'm going to write a little bit more).

<!--more-->

## Generating paginated pages

Let's set up basic Paginate and then extend it. If you are already familiar
with Paginate, you can skip this section.

The data type `Paginate` holds mapping from page numbers to `Identifier`s. The
`buildPaginateWith` function is used to create `Paginate` instances.  This
function has three parameters:

- grouper -- a function to group a list of `Identifier`s into chunks
- pattern -- a `Pattern` instance to get required items
- makeId -- a function to map page numbers to `Identifier`s

Let start with `makeId` as the simplest one:

```Haskell
postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"
```

Pretty straightforward, right? It creates an `Identifier` from the file path. If it's
the first page, the path is just "index.html", otherwise it should take
"index.html" file from the folder named with a page number, e.g. "2/index.html".

The grouper has the following signature:

```Haskell
postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
```

Hakyll provides the `paginateEvery` function which takes a list and groups it into
chunks of a specified number of elements.  Another useful function is
`sortRecentFirst`, which sorts a list of `Identifier`s.  So, we need to sort a
list of identifiers and then paginate it.  To do that, we need to lift the
`paginateEvery` function to the `MonadMetadata` monad.

```Haskell
import Control.Monad (liftM)

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 10) . sortRecentFirst 
```

Now we can build a `Paginate` instance:

```Haskell
main = checkArgs <$> getArgs >>=
        \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do

    paginate <- buildPaginateWith postsGrouper postsPattern postsPageId
```

In a simple case `postsPattern` can be just a string `"posts/*"`, but I use different
configurations depending on the command line parameters (see [Draft posts with
Hakyll](/posts/2015-10-31-Draft-posts-with-Hakyll.html) post).

Once we obtain a `Paginate` instance we can generate content using the
`paginateRules` function.  This function takes an instance of `Paginate` as a
parameter and a function with the following signature `PageNumber -> Pattern ->
Rules()` as a second parameter.

```Haskell
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
```

Hakyll has the `paginateContext` function which returns a context with a
number of fields for the specific page.  As I said before, this is not enough for
me, so I'm using `paginateContextPlus`, which is defined in the next section.

## Extending paginate context

To create the paginator I need a list of all the pages, and a current page.  Since it is
not possible to perform an equality check inside the template, I decided to split
the list of pages into two parts: pages before the current one, and pages after it.
Let's write a function which will create a context with these fields:

```Haskell
import qualified Data.Map as M

paginateContextPlus :: Paginate -> PageNumber -> Context a
paginateContextPlus pag currentPage = paginateContext pag currentPage <> mconcat
    [ listField "pagesBefore" linkCtx $ wrapPages pagesBefore
    , listField "pagesAfter"  linkCtx $ wrapPages pagesAfter
    ]
    where
```

I included `paginateContext` into the result of this function.  Since
we need a lists of pages, we need to use the `listField` function to create
contexts.  We need to create nested contexts for the list elements:

```Haskell
        linkCtx :: Context (String, String)
        linkCtx = field "pageNum" (return . fst . itemBody) <>
                  field "pageUrl" (return . snd . itemBody)
```

The next step is to get the information about each page except the current one and split the
list of the pages into two parts:

```Haskell
        pages = [pageInfo n | n <- [1..lastPage], n /= currentPage]
        lastPage = M.size . paginateMap $ pag
        pageInfo n = (n, paginateMakeId pag n)

        (pagesBefore, pagesAfter) = span ((< currentPage) . fst) pages
```

And the last required part is a `wrapPages` function, which converts
`[(PageNumber, Identifier)]` into `Compiler [Item (String, String)]`. Let's
start with a helper function to convert a single list item into `Compiler (Item
(String, String))`.

```Haskell
        makeInfoItem :: (PageNumber, Identifier) -> Compiler (Item (String, String))
        makeInfoItem (n, i) = getRoute i >>= \mbR -> case mbR of
            Just r  -> makeItem (show n, toUrl r)
            Nothing -> fail $ "No URL for page: " ++ show n
```

Now, we can map a list with `makeInfoItem`:

```Haskell
        wrapPages :: [(PageNumber, Identifier)] -> Compiler [Item (String, String)]
        wrapPages = sequence . map makeInfoItem
```

Since `Compiler` is a monad, we can convert a list of `Compiler`s to the
`Compiler` of the list with the `sequence` function.

## Template

Now, when we have all the required variables in the context, it is simple to add the
paginator to the template:

```html
<nav aria-label="Page navigation">
  <ul class="pagination">
    $if(previousPageNum)$
    <li>
    $else$
    <li class="disabled">
    $endif$
    $if(previousPageNum)$
      <a href="$previousPageUrl$">
    $else$
      <a href="#">
    $endif$
        <span aria-hidden="true">&laquo;</span>
      </a>
    </li>
    $for(pagesBefore)$
    <li><a href="$pageUrl$">$pageNum$</a></li>
    $endfor$
    <li class="active">
      <a href="#">$currentPageNum$<span class="sr-only">current</span></a>
    </li>
    $for(pagesAfter)$
    <li><a href="$pageUrl$">$pageNum$</a></li>
    $endfor$
    $if(nextPageNum)$
    <li>
    $else$
    <li class="disabled">
    $endif$
    $if(nextPageNum)$
      <a href="$nextPageUrl$">
    $else$
      <a href="#">
    $endif$
        <span aria-hidden="true">&raquo;</span>
      </a>
    </li>
  </ui>
</nav>
```

Hakyll requires you to check if the previous/next page variables are defined,
because they are not defined on the first/last page.

And that's all.
