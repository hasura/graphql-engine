# Hasura fork

This is a fork of the package
[resource-pool](https://hackage.haskell.org/package/resource-pool)
with the following changes:

- `createPool'` allows setting a timeout on resource acquisition.
- `getInUseResourceCount` counts the number of resources in use.

---

# Welcome to pool

pool is a fast Haskell library for managing medium-lifetime pooled
resources, such as database connections.

# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/bos/pool/issues).

Master [git repository](http://github.com/bos/pool):

* `git clone git://github.com/bos/pool.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/pool):

* `hg clone http://bitbucket.org/bos/pool`

(You can create and contribute changes using either git or Mercurial.)

Authors
-------

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
