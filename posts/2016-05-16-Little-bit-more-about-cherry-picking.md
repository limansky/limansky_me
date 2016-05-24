---
title: Little bit more about git cherry-pick
tags: git
---

In [one of the previous posts](/posts/2015-12-16-Batch-cherry-picking-in-git.html)
I told about a way to copy a bunch of commits from one git branch to another one.
Today I did it again, but for the other reason.

Several days ago I started a new project in a new repository.  I made several
commits on `master` when I realized what I need to create a pull request to
perform a code review.  What I had:

```
A -- B -- C -- D -- E                                          master
```

What I'd like to have:

```
X                                                              master
 \
  A -- B -- C -- D -- E                                        feature
```
<!--more-->

Since it was a new repository I didn't care about the commit history.  First of
all I created a new detached branch to have a clear history:

```
$ git checkout --orphain temp
$ git rm -rf .
$ vim .gitignore
$ git add .gitignore
$ git commit -m "Initial commit"
```

Now my history looks like this:
```
A -- B -- C -- D -- E                                          master

                        X                                      temp
```

So there was two histories, not related with each other.  Next step was to
create feature branch and copy commits there.

```
$ git checkout -b feature
$ git rev-list --reverse master | git cherry-pick --stdin
$ git push -u origin feature
```

Since it was an empty branch, there was no conflicts.  Another difference between
this command and a command from the previous post, was what I didn't use a commit range,
but a whole branch as an argument for the `rev-list` command. I was almost there:


```
A -- B -- C -- D -- E                                          master

                        X                                      temp
                         \
                          A' -- B' -- C' -- D' -- E'           feature
```

The final step was to set the `master` branch to the commit `X` and remove the
`temp` branch.

```
$ git checkout master
$ git reset --hard temp
$ git branch -d temp
$ git push -f origin master
```
