---
title: Putting posts content on the index page
tags: Hakyll, Haskell
---

Once you have created a Hakyll based site the first page contains only the
titles of your posts.  It can be acceptable if your first page contains another
content, but if there are only the latest posts it looks weird.  Let's check
how does it works.

Default index page template includes `post-list.html` template which contains
following code:

``` html
<ul>
  $for(posts)$
    <li>
      <a href="$url$">$title$</a> - $date$
    </li>
  $endfor$
</ul>
```

<!--more-->

You could change this code, but you should remember that archives page uses
same template for rendering.  So, it would be better to create separate snippet
for the complete post list. This template will be really simple:

``` html
<ul>
  $for(posts)$
    <li class="post-preview">
      <a href="$url$">$title$</a> - $date$
      $body$
    </li>
  $endfor$
</ul>
```

But where the `posts` came from?  Definitely from the Haskell code.  The initial
`site.hs` has following code to fill the index page:


``` Haskell
match "pages/index.html" $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
                listField "posts" postCtx (return posts) `mappend`
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
```

Actually we don't want to have complete post here, but only the post content.
We can achieve it using snapshots.  So, instead of using `loadAll` we can get
it from snapshot prepared on rendering posts:

``` Haskell
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
```

So, you need to save snapshot when you processing your posts:

``` Haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/post-with-comment.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

Finally, it would be nice to have a post preview.  Fortunately, this
functionality is already implemented in Hakyll.  In your blog post you need to
put `<!--more-->` marker in the place you'd like to stop preview.  When you
need to use `teaserField` function which takes name of teaser variable and name
of the snapshot to use and creates `Context String`.  Since `Context` is a
Monoid you can combine it with default post context using `mappend`.

``` Haskell
    match "pages/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" previewCtx (return posts) `mappend`
                    defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

previewCtx :: Context String
previewCtx = teaserField "preview" "content" `mappend` postCtx
```

And you also need to update the template to make it preview aware:

``` html
<ul>
  $for(posts)$
    <li class="post-preview">
      <a href="$url$">$title$</a> - $date$
      $if(preview)$
        $preview$
      $else$
        $body$
      $endif$
    </li>
  $endfor$
</ul>
```

And that's it.  Of course there a lot of things are still missed, like tags and
pagination.  I'm going to cover it in the next posts as soon as I'll add them
to this blog.
