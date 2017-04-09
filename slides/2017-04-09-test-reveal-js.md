---
title: Reveal JS test
subtitle: Getting started with Hakyll and RevealJS
author: Mike Limansky
theme: beige
---

# Two vertical slides

## Just an example slide

List

* this 
* is
* cool

## Second vertical slide

Bla-bla-bla.

Very important information.

---

## Supported variables

The following variables are passed via Hakyll Context

* title
* subtitle
* theme
* transition
* author
* date

## Some code

It's possible to insert some code:

```Haskell
revealJsWriterOptions :: String -> WriterOptions
revealJsWriterOptions template = defaultHakyllWriterOptions
    { writerTemplate = template
    , writerSectionDivs = True
    , writerStandalone = True
    , writerHtml5 = True
    }
```
