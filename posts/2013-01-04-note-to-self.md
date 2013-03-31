---
title: Note to self
author: Janne Hellsten
date: January 4, 2013
public: true
---

Note to self: Don't use SQLite3 from multiple Haskell threads -- at
least not with
[sqlite-simple](http://hackage.haskell.org/package/sqlite-simple).

I recently implemented a To-Do app (the "Hello World" of web apps)
using [AngularJS](http://angularjs.org/) on the frontend and
[Snap](http://snapframework.com/) on the backend.  If you haven't
heard about AngularJS, I highly recommend you to check it out -- it's
got a very clean approach to developing modern single-page web apps.

In my app, the Snap-based server was used mainly for authentication
and persistence.  The rest of the app runs client-side in JavaScript
and talks to the server over a simple REST API.  Getting all this up
and running was surprisingly easy.

Anyhow..  Turns out it's easy to write an AngularJS app that sends a
lot of concurrent requests to the server, even in the single user
case.  As an example, you can iterate through a list of todo-items in
JavaScript, persisting each item with a call to `item.$save()`:

~~~~~{.javascript}
    $scope.archive = function() {
        var oldTodos = $scope.todos;
        $scope.todos = [];
        angular.forEach(oldTodos, function(todo) {
            if (!todo.done)
                $scope.todos.push(todo);

            todo.$save();
        });
    };
~~~~~

Each `$save()` triggers an AJAX call and the server will see these as
multiple concurrent requests.

Unfortunately though, I started seeing lot of SQLITE_BUSY errors on
the server side with this usage.  Turns out accessing a single SQLite3
database from multiple threads, each with its own connection is not as
simple as I thought.  If you're in the middle of reading rows from a
SELECT and you issue an INSERT from another connection, the INSERT
will fail with an SQLITE_BUSY error.  Here's Haskell code that
reproduces this problem:


~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}

module Direct (
  directSqliteTest
  ) where

import Control.Monad
import Database.SQLite3.Direct

directSqliteTest :: IO ()
directSqliteTest = do
  Right conn1 <- open "test.db"
  Right conn2 <- open "test.db"

  exec conn1 "INSERT INTO a (text) VALUES ('foo')"
  exec conn1 "INSERT INTO a (text) VALUES ('foo')"

  Right (Just sr) <- prepare conn1 "SELECT * from a"
  a <- step sr
  a <- step sr
  print a
  x <- exec conn2 "INSERT INTO a (text) VALUES ('foo')"
  -- ^^ SQLITE_BUSY is triggered
  finalize sr
  print x

  void $ close conn1
  void $ close conn2
~~~~~

The above code will print:

```
$ ./dist/build/sqlite-test/sqlite-test
Right Row
Right Row
Left (ErrorBusy,Utf8 "database is locked")
```

As
[snaplet-sqlite-simple](http://hackage.haskell.org/package/snaplet-sqlite-simple)
does connection pooling, the above pattern is very likely to happen.
The SQLite snaplet maintains a connection pool which is used to
service connections to request handlers.  Concurrent requests will
each be handed their own connection instead of using a single shared
connection.  A request can be in the middle of reading database rows
from the while another request tries to write to the database using
another connection, and *boom*, the above SQLITE_BUSY scenario
triggers.

As a quick band-aid fix, I changed snaplet-sqlite-simple to not use a
connection pool but instead allocate a single database handle on init
and servicing the same handle sequentially to anyone that asks
([commit 3957f722][mvar-commit]).  For extra safety, I also stuck the
connection inside an `MVar` so that all SQLite operations get
serialized within the Snap application.

Hopefully I'll be able to switch to a more concurrent model in the
future, but at least the current v0.4.0 release works without
SQLITE_BUSY errors.

 [mvar-commit]: https://github.com/nurpax/snaplet-sqlite-simple/commit/3957f722cce6abf7e1059f481668379e05b62286
