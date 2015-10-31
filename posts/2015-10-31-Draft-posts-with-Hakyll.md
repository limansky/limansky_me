---
title: Draft posts with Hakyll
tags: Hakyll, Haskell
---

If you'd like to write a long posts in your Hakyll blog, your often can found
that I'd like to commit a post (or several posts), but your are not going to
deploy it on server.  Here is my solution inspired by [this
post](http://www.blaenkdenum.com/posts/drafts-in-hakyll/).  Now I can call
Hakyll with `--with-drafts` and it will use both `posts` and `drafts`
directories to collect posts data.

There are several problems to solve.

0. Detect what we are in the draft mode.
1. Pass posts to Hakyll.
2. It is safer to have separate folder for generated site in draft mode to
   avoid deploy of draft articles by mistake.
3. Hakyll itself uses [cmdargs](https://hackage.haskell.org/package/cmdargs)
   package which checks if the command is executed with proper options.

<!--more-->

Passing posts to Hakyll
-----------------------

This is the easier one. Hakyll uses patterns to match your blog posts to
process them.  By default it's `"posts/*"`.  In many cases it is used in
several places, so it's a really good idea to define a variable.  Patterns can
be combined using `.||.` operator.  So we can have something like:

```Haskell
main = do
  let postsPattern = if (draftMode)
    then "posts/*" .||. "drafts/*"
    else "posts/*"

  hakyll $ do
    match postsPattern $ do
      route $ setExtension "html"
      compile $ pandocCompiler
-- other routes implementation
```

Draft configuration
-------------------

To put result into another directory we need to change configuration and pass
it to Hakyll.

```Haskell
main = do
  let config = if (draftMode) draftConfiguration else defaultConfiguration

  hakyllWith config $ do
    match postsPattern $ do
-- routes implementation

-- Configuraiton for draft
draftConfiguration = defaultConfiguration {
        destinationDirectory = "_draftSite"
      , storeDirectory = "_draftCache"
      , tmpDirectory = "_draftCache/tmp"
      }

```

Putting all together
--------------------

So far so good, but we still not detect if it's a draft mode or not.  All we
need is to process command line arguments:

```Haskel
main = do
  draftMode <- fmap (elem "--with-drafts") getArgs
```

Unfortunately, this is not enough.  As I mentioned before, cmdargs does not
allow to pass unknown options, so we need to filter out all our staff before
passing it to hakyll.  The good news is that we can process arguments and
replace them using `withArgs` function. So the final solution is looking like
that:

```Haskell
checkArgs :: [String] -> (Pattern, Configuration, [String])
checkArgs args = case partition (/= "--with-drafts") args of
    (_, []) -> ("posts/*",                  defaultConfiguration,   args)
    (as, _) -> ("posts/*" .||. "drafts/*",  draftConfiguration,     as)

main = checkArgs <$> getArgs >>= 
        \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do
    match postsPattern $ do
-- routes implementation
```
