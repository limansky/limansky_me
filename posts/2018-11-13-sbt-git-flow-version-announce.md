---
title: sbt-git-flow-version announcement
tags: Scala, sbt, git, sbt-git-flow-version
---

I'd like to announce my new project called [sbt-git-flow-version](https://github.com/limansky/sbt-git-flow-version).
As you might guess it's a plugin for sbt.  The goal of the project is set your
sbt build version according to [git flow](https://leanpub.com/git-flow/read) rules.

I found that for big or medium size teams git flow is quite practical.  What I didn't
like about it, is that it requires to change the version all the time you change the branch
in your repository.  At some point I took responsibilities of release engineer in our team.
Since I'm very lazy to do all of this styff I came up with some Scala code in the `project` catalog
of my current working project. Later I rewrote it as an sbt plugin.

So, what can it do for you?

<!--more-->

Let's start with git flow.  The typical git flow process have following branches:

 * `master` - contains stable releases. Only tested releases are merged here, and only 
   stable artifacts are build from this branch (no snapshots allowed).
 * `develop` - current development release. Features for the next release are going here.
   We can define the version as a next minor version: "(last version + 0.1)-SNAPSHOT".
 * `release/x.y.z` - release branches. These branches are for upcoming releases during
   stabilization.  In our team we do not have `develop` branch, but several `release`
   branches.  For me, it's more clear.  The versions on this branch is `x.y.z-SNAPSHOT`.
 * `feature/xxx`, and also `bugfix/yyy`, `hotfix/zzz` - branches for a specific task
   development.  The version for such build 

You might see that we need three components to compose the version:

 * previous version
 * current tags
 * current branch name

The main setting of sbt-git-flow-version is `versionPolicy` which has type
`Seq[(BranchMatcher => VersionCalculator)]`.  So it tries to find matching rule for
a branch and calculate a version.

What is a `BranchMatcher`?  It's a function `String => Option[Matching]` where matching
is a case class containing branch name and some additional optional extracted string.  There are several
predefined branch matchers:

 
 * `exact(name: String)` - branch name is equals to `name`, extraction is empty.
 * `prefix(prefix: String)` - branch name starts with `prefix`.  The part after prefix is extraction.
 * `prefixes(prefixes: String*)` - same as prefix but supports a number of prefixes.
 * `regex(r: String)` - branch name matches regular expression `r`.  If the expression contains
   groups, the first group will be returned as extraction.
 * `any` - matches any branch (might be used to define default behaviour).

The `VersionCalculator` converts previous version, current version, and matching either
to the version number or error message.  The predefined version calculators are:

 
 * `currentTag` - take a version from a current tag. If there are several current tags it will take the
   one with a maximal version.
 * `nextMajor`, `nextMinor`, `nextBuild`, `nextN` - increment major, minor, build or n-th number of
   a last version. The version is snapshot by default.
 * `matching` - take a version from matching returned by `BranchMatcher`. The version is snapshot by default.
 * `lastVersion` - previous version.  Version value is taken from last tag or `initialVersion` setting.
   The version is not snapshot by default.
 * `lastVersionWithMatching` - takes last version and append matching returned by `BranchMatcher`.  By default
   new version is snapshot.
 * `unknownVersion` - fails with unknown version message.

The default policy is defined as:

```Scala
Seq(
  exact("master") -> currentTag(),
  exact("develop") -> nextMinor(),
  prefix("release/") -> matching(),
  prefixes("feature/", "bugfix/", "hotfix/") -> lastVersionWithMatching(),
  any -> unknownVersion
)
```

What literally means following:

 * If branch name is "master" then the branch name is a current commit tag
 * If branch name is "develop" then the branch name is a next minor version.
   E.g. if last version was `1.4.2`, the current version is `1.5.0-SNAPSHOT`.
 * If the branch name starts with "release/" then the version is taken from the
   branch name.  E.g. for the branch `release/2.12.85` the version is `2.12.85-SNAPSHOT`.
 * For the branches started with "feature/", "bugfix/", and "hotfix/" the version is
   a combination of the last version and matching.  E.g. for the branch `feature/123-new-ui`
   and the prevous version "1.0.1" the current version is `1.0.1-123-new-ui-SNAPSHOT`.
 * Finally, if the branch name doesn't follow any of these rules, the build fails,
   because version is unknown.

You can define your own rules in you `build.sbt` in the following way:

```
import sbtgitflowversion.BranchMatcher._
import sbtgitflowversion.VersionCalculator._

versionPolicy := (exact("big-release") -> next-major()) +: versionPolicy.value
```

This will set the next major version for the "big-release" branch.

Current verion of the plugin is `0.1`.  So, to add to your `project/plugins.sbt` the line:

```
addSbtPlugin("me.limansky" % "sbt-git-flow-version" % "0.1")
```
