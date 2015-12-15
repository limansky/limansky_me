---
title: Batch cherry-picking with git
tags: git
---

Today I came to work and found that I have to move 18 commits from my
development git branch to the old release.  I've worked on one feature for
several weeks, and kept my branch up to date with master, so the history looked
like this:

```
               Z -- Y -- X -- W -- V -- U -- T -- S            feature
              /         /              /
-- A -- B -- C -- D -- E ----- F ---- G -- H                   master
    \
     R -- Q                                                    release
```

So, my goal is to create a `feature'` branch on top of the `release` and copy the
commits `Z`, `Y`, `W`, `V`, `T` and `S` there:

<!--more-->

```
               Z -- Y -- X -- W -- V -- U -- T -- S            feature
              /         /              /
-- A -- B -- C -- D -- E ----- F ---- G -- H                   master
    \
     R -- Q                                                    release
           \
            Z' -- Y' -- W' -- V' -- T' -- S'                   feature'
```

I was not able to rebase my working branch. So I had to `cherry-pick` the
commits one by one from my working branch to the new branch, based on release
version.  I'm not very meticulous man.  To be honest I'm quite careless, so I
really didn't want to do it by hand one by one.  Fortunately, `git cherry-pick`
allows you to pass several commits, or to pass the range. But the problem is to
get only related commits, and skip commits from the master and merges. The
`cherry-pick` command itself doesn't have this function, but it can take
commits from another command using `--stdin` option.

The powerful command to get different commit lists is `git rev-log`.  You can
use `--grep` option to filter the commits (I used bug tracker id, but it can be
used with any other criteria).  Since we need to apply commits in chronological
order, `--reverse` option shall be specified.  And the last, but lot least
required option `--no-merge` is required to avoid merge-commits in the list.
Now the list of commits can be passed to `git cherry-pick` command:

```
git rev-log --reverse C..S --grep='featureX' --no-merge | git cherry-pick --stdin
```

If any conflict occurs, you have to solve it (using `git mergetool` or manually
with `git add`) and then run `git cherry-pick --continue` until all commits are
copied.

Finally, I you are going to merge the `release` branch into your master, it
might be a good idea to merge original `feature` branch into `master` first.
It will simplify the merge with `release` since all conflicts between `master`
and `feature` branches are already resolved.

```
               Z -- Y -- X -- W -- V -- U -- T -- S            feature
              /         /              /           \
-- A -- B -- C -- D -- E ----- F ---- G -- H ------ K -- L     master
    \                                                   /
     R -- Q -- Z' -- Y' -- W' -- V' -- T' ------------ S'      feature'
```
