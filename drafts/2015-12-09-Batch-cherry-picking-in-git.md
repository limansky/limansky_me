---
title: Batch cherry-picking with git
tags: git
---

Today I came to work and found that I have to move 18 commits from my
development git branch to the old release.  I've worked on one feature for
several weeks, and kept my branch up to date with master, so the history was
looking like this:

```
               Z -- Y -- X -- W -- V -- U -- T -- S            feature
              /         /              /
-- A -- B -- C -- D -- E ----- F ---- G -- H                   master
    \
     R -- Q                                                    release
```

So, my goal is to create `feature'` branch on top of `release` and move commits
`Z`, `Y`, `W`, `V`, `T` and `S` there:


```
               Z -- Y -- X -- W -- V -- U -- T -- S            feature
              /         /              /
-- A -- B -- C -- D -- E ----- F ---- G -- H                   master
    \
     R -- Q                                                    release
           \
            Z' -- Y' -- W' -- V' -- T' -- S'                   feature'
```

I was not able to rebase my working branch. So I had to `cherry-pick`
commits one by one from my working branch to the new branch, based on release
version.  I'm not very careful man.  To be honest I'm a quite unmindful, so I
really didn't want to do it by hand one by one.  Fortunately, `git cherry-pick`
allows you to pass several commits, or to pass the range. But the problem is to
get only related commits, and skip commits from the master and merges. The
`cherry-pick` command itself doesn't have such functionality, but it can take
commits from another command using `--stdin` option.

The powerfull command to get different commit lists is `git rev-log`.

```
git rev-log --reverse C..S --grep='featureX' --no-merge | git cherry-pick --stdin
```

If any conflict will be occured, you have to solve it and then run `git
cherry-pick --continue` until all commits will be copied.
