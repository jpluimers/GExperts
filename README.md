# GExperts

Synced every now and then from git svn clone https://svn.code.sf.net/p/gexperts/code/trunk

## Branches

- [`master`](https://github.com/jpluimers/GExperts/tree/master) the latest sync from SVN
- [`develop`](https://github.com/jpluimers/GExperts/tree/develop) the latest merge of my work and the SVN; usually quite accurate, but might not build
- [`feature` sub-branches](https://github.com/jpluimers/GExperts/branches) contain my changes, usually numbered (the number correspondes to the issue number it tries to address)

Note that the **default** branch is `develop`, which makes it easier for me to merge stuff.

### Pull requests

If you like to cooperate on GExperts, but are not a fan of SVN, you can submit a pull request relative to the `develop` branch and I will try to forward it to Thomas Mueller, who maintains GExperts no SVN based SourceForge.

## Sync instructions

For myself to know the steps for syncing this repository from SVN.

## First time 

1. `git svn clone https://svn.code.sf.net/p/gexperts/code/trunk GExperts.git-svn`
2. `git remote add origin https://github.com/jpluimers/GExperts.git` 
3. `git push origin master`

## Each additional time

From inside the `GExperts.git-svn` directory: 

1. `git checkout master`
2. `git svn fetch`
3. visually inspect the changes
4. `git svn rebase`
5. visually inspect the rebase
6. `git push origin master`
