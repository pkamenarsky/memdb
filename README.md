# memdb

[![CircleCI](https://circleci.com/gh/pkamenarsky/memdb.svg?style=svg)](https://circleci.com/gh/pkamenarsky/memdb)

This package contains a thin wrapper over a continous memory region combined
with an efficient reverse index implementation for fast and type-safe indexed
lookups.

It is aimed at storing, loading and querying big immutable datasets. Once
written, a database can not be modified further.

The underlying storage is pinned and thus ensures efficient garbage
collection without ever reading the structure contents, since no pointers
live inside the dataset that point outside it.
