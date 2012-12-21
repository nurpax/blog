---
title: Snap with SQLite3
author: Janne Hellsten
date: December 20, 2012
description: On snaplet-sqlite-simple and its example code
---

I recently released [snaplet-sqlite-simple
0.3](http://hackage.haskell.org/package/snaplet-sqlite-simple), a
compatibility update for the recently released [Snap
0.10](http://hackage.haskell.org/package/snap).  This library is glue
for hooking up a Snap application to an SQLite3 database using the
[sqlite-simple](http://hackage.haskell.org/package/sqlite-simple)
library.  Using SQLite with Snap is a handy, light-weight approach for
quickly prototyping database-backed web apps -- you don't need to
configure a separate SQL server just to run your web app.


While the new snaplet-sqlite-simple 0.3 release doesn't add any
significant new functionality, I thought a few things in its [example
project](https://github.com/nurpax/snaplet-sqlite-simple/tree/master/example/src)
would be worth a mention.

This example project implements a simple web app that has a login
screen (with new user registration) and a main page where logged in
users can drop comments.  Users and comments are both persisted into
an SQLite3 database.

Here's a couple of screenshots to show how it looks like.  Let's start
with the login screen:

<div class="screenshot white-bg">
![](/images/snaplet-sqlite-simple-example-login.png "Login screen")
</div>

If you create a new user and login to the app, you're taken to the
main page.  Here the logged in user can add comments.  The comments
get persisted into a database and are associated with the current user
(e.g., other users can't see them.)  Here's how the main page looks
like:

<div class="screenshot white-bg">
![](/images/snaplet-sqlite-simple-example-comments.png "Main page")
</div>

The example demonstrates a few basic concepts in a complete example:

* Connecting to an SQLite database
* Creating database tables (if not created) on web app's init
* Associating your own user data with Snap's
  [Snap.Snaplet.Auth](http://hackage.haskell.org/packages/archive/snap/0.10.0.1/doc/html/Snap-Snaplet-Auth.html)
  user objects
* Making simple database queries and rendering the results on the main screen

Setting up the connection is easy, see the `app` function in
[Site.hs](https://github.com/nurpax/snaplet-sqlite-simple/blob/master/example/src/Site.hs).

Creating the database schema on app startup is perhaps less obvious.
This also happens on app init:

~~~~~{.haskell}
-- | The application initializer.
app :: SnapletInit App App
-- ...
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let connPool = sqlitePool $ d ^# snapletValue
    liftIO $ withResource connPool $ \conn -> Db.createTables conn
~~~~~

The actual table creation is done in
[Db.hs](https://github.com/nurpax/snaplet-sqlite-simple/blob/master/example/src/Db.hs).
This module also contains query functions for saving and listing
comments for a given user.

Everything on the main page requires a logged in user.  Requiring a
logged in user is ensured using `withLoggedInUser` which either calls
a handler with the currently logged in user or redirects to the login
screen.  It's defined as:

~~~~~{.haskell}
-- | Run actions with a logged in user or go back to the login screen
withLoggedInUser :: (Db.User -> H ()) -> H ()
withLoggedInUser action =
  currentUser >>= go
  where
    go Nothing  = handleLogin (Just "Must be logged in to view the main page")
    go (Just u) = maybeWhen (userId u) (action . user)
      where
        user uid = Db.User (read . T.unpack $ unUid uid) (userLogin u)
~~~~~

You can use it anywhere you need to access the currently logged in
user.  For example, here's how the main page handler deals with the
current user:

~~~~~{.haskell}
mainPage :: H ()
mainPage = withLoggedInUser go
  where
    go :: Db.User -> H ()
    go user = do
      comments <- withTop db $ Db.listComments user
      heistLocal (splices comments) $ render "/index"
    splices cs =
      I.bindSplices [("comments", I.mapSplices renderComment cs)]
~~~~~

To play more with this code, I recommend that you clone the code from
Git, try to build the example and play with the app:


~~~~~{.bash}
git clone git://github.com/nurpax/snaplet-sqlite-simple.git
cd example
cabal-dev install
./cabal-dev/bin/example
# browse to http://localhost:8000
~~~~~
