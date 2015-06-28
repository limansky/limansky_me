---
title: Installing Hakyll on Gentoo Linux
tags: Hakyll, Gentoo
---

I have been using Gentoo Linux as my primary OS since 2004.  So, it usually
important for me to choose software which available in [Gentoo
portage](http://packages.gentoo.org).  And there is no Hakyll in portage :(  

Of course, it is possible to install Hakyll from cabal using `cabal-install`,
but according to the [wiki](https://wiki.gentoo.org/wiki/Haskell#Cabal) this is
not recommended.  Instead of this, we can install this package fro the
[Gentoo Haskell](https://github.com/gentoo-haskell/gentoo-haskell) overlay.  So,
first of all it required to add this overlay using layman (of course if you
didn't do it before).

```
$ emerge layman
$ layman -a haskell
$ echo "/var/lib/layman/make.conf" >> /etc/portage/make.conf
```

The next problem is that Hakyll requires Pandoc version >= 1.14 (which is also
available in the overlay), and it has dependency on GHC >= 7.8.  And this means
that you have to unmask lot of packages.  Bad news: autounmasking doesn't work.
Lot of packages should be unmasked manually to avoid blocking. Good news: I've
done that and here is the list of unmasked packages:

```
=dev-haskell/aeson-0.9.0.1 ~amd64
=dev-haskell/hourglass-0.2.9 ~amd64
=dev-haskell/cmark-0.3.4 ~amd64
=dev-haskell/bytestring-mmap-0.2.2-r1 ~amd64
=dev-haskell/x509-store-1.5.0 ~amd64
=dev-haskell/http-client-0.4.13 ~amd64
=dev-haskell/tls-1.2.18 ~amd64
=dev-haskell/texmath-0.8.2 ~amd64
=dev-haskell/conduit-1.2.4.2 ~amd64
=dev-haskell/pandoc-types-1.12.4.4 ~amd64
=dev-haskell/yaml-0.8.11 ~amd64
=dev-haskell/contravariant-1.3.1.1 ~amd64
=dev-haskell/enclosed-exceptions-1.0.1.1 ~amd64
=dev-haskell/tagsoup-0.13.3-r1 ~amd64
=dev-haskell/http-client-tls-0.2.2 ~amd64
=dev-haskell/transformers-0.4.3.0 ~amd64
=dev-haskell/enumerator-0.4.20 ~amd64
=dev-haskell/transformers-compat-0.4.0.4 ~amd64
=dev-haskell/bytestring-builder-0.10.6.0.0 ~amd64
=dev-haskell/xml-conduit-1.3.0 ~amd64
=dev-haskell/zip-archive-0.2.3.7 ~amd64
=dev-haskell/exceptions-0.8.0.2 ~amd64
=app-text/pandoc-1.14.0.4 ~amd64
=dev-haskell/fsnotify-0.1.0.3 ~amd64
=dev-haskell/x509-1.5.1 ~amd64
=dev-haskell/x509-validation-1.5.2 ~amd64
=dev-haskell/hinotify-0.3.7 ~amd64
=dev-haskell/highlighting-kate-0.6 ~amd64
=dev-haskell/haddock-library-1.1.1 ~amd64
=dev-haskell/deepseq-generics-0.1.1.2 ~amd64
=dev-haskell/unordered-containers-0.2.5.1 ~amd64
=dev-haskell/attoparsec-0.13.0.0 ~amd64
=dev-haskell/snap-server-0.9.5.1-r1 ~amd64
=dev-haskell/hakyll-4.7.0.0-r1 ~amd64
=dev-haskell/streaming-commons-0.1.12.1 ~amd64
=dev-haskell/filemanip-0.3.6.3 ~amd64
=dev-haskell/transformers-base-0.4.4 ~amd64
=dev-haskell/zlib-enum-0.2.3.1 ~amd64
=dev-haskell/connection-0.2.4 ~amd64
=dev-haskell/asn1-parse-0.9.1 ~amd64
=dev-haskell/statevar-1.1.0.0 ~amd64
=dev-haskell/semigroups-0.16.2.2 ~amd64
=dev-haskell/blaze-builder-enumerator-0.2.1.0 ~amd64
=dev-haskell/resourcet-1.1.5 ~amd64
=dev-haskell/snap-core-0.9.7.0 ~amd64
=dev-haskell/crypto-pubkey-types-0.4.3 ~amd64
=dev-haskell/pandoc-citeproc-0.7.1.1-r1 ~amd64
=dev-haskell/scientific-0.3.3.8 ~amd64
=dev-haskell/blaze-builder-0.4.0.1 ~amd64
=dev-haskell/attoparsec-enumerator-0.3.4 ~amd64
=dev-haskell/time-locale-compat-0.1.1.0 ~amd64
=dev-haskell/lrucache-1.2.0.0 ~amd64
=dev-haskell/conduit-extra-1.1.9 ~amd64
=dev-haskell/monadcatchio-transformers-0.3.1.3 ~amd64
=dev-haskell/sha-1.6.4.2 ~amd64
=dev-haskell/crypto-pubkey-0.2.8 ~amd64
=dev-haskell/x509-system-1.5.0 ~amd64
=dev-haskell/cipher-des-0.0.6 ~amd64
=dev-haskell/http-conduit-2.1.5 ~amd64
=dev-haskell/system-fileio-0.3.16.3 ~amd64
=dev-haskell/asn1-types-0.3.0 ~amd64
=dev-haskell/asn1-encoding-0.9.0 ~amd64
=dev-haskell/juicypixels-3.2.5.2 ~amd64
=dev-haskell/network-2.6.0.2 ~amd64
=dev-haskell/network-uri-2.6.0.3 ~amd64
=dev-haskell/cabal-1.22.4.0 ~amd64
=dev-haskell/cabal-install-1.22.4.0 ~amd64
=dev-lang/ghc-7.8.4-r4 ~amd64
=dev-haskell/old-locale-1.0.0.7 ~amd64
=dev-haskell/old-time-1.1.0.3-r1 ~amd64
=dev-haskell/mmorph-1.0.3 ~amd64
=dev-haskell/parsec-3.1.5 ~amd64
=dev-haskell/mtl-2.2.1 ~amd64
=dev-haskell/http-4000.2.18 ~amd64
=dev-haskell/monad-control-0.3.3.0 ~amd64
=dev-haskell/system-filepath-0.4.9 ~amd64
=dev-haskell/http-types-0.8.6 ~amd64
=dev-haskell/blaze-html-0.8.0.2 ~amd64
=dev-haskell/blaze-markup-0.7.0.2 ~amd64
=dev-haskell/case-insensitive-1.1.0.3 ~amd64
=dev-haskell/vector-0.10.11.0 ~amd64
```

You might have a trouble with blocking if you already have installed GHC.  In
this case you should `emerge -uD ghc` and then call `haskell-updater --upgrade`
before emerging Hakyll.

Now you can create your own Hakyll based site.
