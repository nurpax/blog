---
title: AngularJS with Haskell
author: Janne Hellsten
date: January 13, 2013
---

I recently wrote a simple To-Do app in [AngularJS][angularjs] using
the [Snap Framework][snap].  I started with the To-Do example from
[AngularJS's homepage][angularjs] and added the necessary JavaScript
plumbing to enable talking to a Haskell web server over a REST API
(mind you, not in the [strict sense][REST], just enough to have the
client and server to talk to each other in JSON.)  I figured someone
might find my example a useful starting point for developing web
applications with these technologies, so I uploaded the [code on
github][example] and decided to highlight a few points about it with
this blog post.

I'm not going into much detail about AngularJS usage -- you should
read more about it on their [homepage][angularjs].  This post will
focus on setting things up so that the AngularJS app can successfully
work with a backend written in Haskell.

The basic architecture of the app is that all UI action happens on
client-side (frontend), mostly written in JavaScript.  (This includes
most of HTML templating too.)  The Haskell backend takes care of
authentication and persistence.  The frontend talks to the backend via
a simple REST API.  The REST API communicates things like listing,
modifying or adding to-do items.

The main client-side source files are:

* [static/index.html]: Main page
* [static/todo.js]: Controller for To-do items
* [static/services.js]: Client-side model definition

Let's start with the client-side.  The "AngularJS application" is
defined [static/index.html].  It contains definitions on how to
display the main page, how to list to-do items and how to hook up UI
events on to-dos to controller actions.

The application calls into a `TodoCtrl` controller defined in
[static/todo.js].  The controller is mostly about taking action on UI
events.  The controller talks to the backend via the `Todo` service.  

The `Todo` service is defined [static/services.js].  It defines how
the client-server interface on accessing user's to-do items, providing
entry points such as `Todo.query()` and `Todo.save()`.


The most interesting server parts can be found in the following source
files:

* [src/Site.hs]: Main server module (login, authentication, REST)
* [src/Db.hs]: Model definition (types, JSON serialization, database persistence)

The model definition of a to-do app is rather simple.  The main type
being the `Todo` item:


```{.haskell}
data Todo =
  Todo
  { todoId :: Maybe Int64   -- database row id
  , todoText :: T.Text      -- todo text
  , todoDone :: Bool        -- completed?
  } deriving (Show)
```

The [Db][src/Db.hs] module defines how these objects can be serialized
to/from JSON using the [aeson package][aeson]:

```{.haskell}
instance FromJSON Todo where
  parseJSON (Object v) =
    Todo <$> optional (v .: "id")
         <*> v .: "text"
         <*> v .: "done"
  parseJSON _ = mzero

instance ToJSON Todo where
  toJSON (Todo i text done) =
    object [ "id" .= fromJust i
           , "text" .= text
           , "done" .= done
           ]
```

The reason for using `Maybe` for `todoId` is to distinguish between
persisted (has a database row id) and unsaved (no row id) to-do items.

The rest of the module is about persisting `Todo`s in an SQLite
database.

The final piece of the puzzle is the [Site][src/Site.hs] module which
plugs all of the above server bits together.  It manages routing,
authentication and serves to-do items over a REST API.  The
server-side routing is defined as follows:

```{.haskell}
-- | Render main page
mainPage :: H ()
mainPage = withLoggedInUser (const $ serveDirectory "static")

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        with auth handleLoginSubmit)
         , ("/logout",       with auth handleLogout)
         , ("/new_user",     with auth handleNewUser)
         , ("/api/todo",     with auth handleTodos)
         , ("/",             with auth mainPage)
         , ("/static",       serveDirectory "static")
         ]
```

There's a couple of things to note about the above routing table:

* The REST interface runs at `/api/todo` and calls into `handleTodos`
  (more about this below)
* Serving the main page is just a matter of serving static files.
  This is because all the important templating happens on client-side
  in [static/index.html].

Implementing the REST API was fairly straightforward, esp. with the
hidden gem that is [snap-extras]:

```{.haskell}
handleTodos :: H ()
handleTodos =
  method GET  (withLoggedInUser getTodos) <|>
  method POST (withLoggedInUser saveTodo)
  where
    getTodos user = do
      todos <- withDb $ \conn -> Db.listTodos conn user
      writeJSON todos

    saveTodo user = do
      newTodo <- getJSON
      either (const $ return ()) persist newTodo
        where
          persist todo = do
            savedTodo <- withDb $ \conn -> Db.saveTodo conn user todo
            writeJSON savedTodo
```

The `handleTodos` function handles either `GET` or `POST` requests,
for retrieving and modifying todo items.  All its request parameters
come in as JSON -- these are turned into Haskell data using `getJSON`
from [Snap.Extras.JSON][snap-extras-json].  Writing out the request
response is done using `writeJSON`.  Automatic JSON encode and decode
is made possible by the `FromJSON` and `ToJSON` instances defined in
the [Db][src/Db.hs] module.


 [angularjs]: http://angularjs.org/
 [aeson]: http://hackage.haskell.org/package/aeson
 [example]: https://github.com/nurpax/snap-examples/tree/master/angularjs-todo
 [sqlite-simple]: https://github.com/nurpax/sqlite-simple
 [snap-extras]: http://hackage.haskell.org/package/snap-extras
 [snap-extras-json]: http://hackage.haskell.org/packages/archive/snap-extras/0.3/doc/html/Snap-Extras-JSON.html 
 [snap]: http://snapframework.com/
 [REST]: http://en.wikipedia.org/wiki/Representational_state_transfer
 [static/index.html]: https://github.com/nurpax/snap-examples/blob/master/angularjs-todo/static/index.html
 [static/todo.js]: https://github.com/nurpax/snap-examples/blob/master/angularjs-todo/static/todo.js
 [static/services.js]: https://github.com/nurpax/snap-examples/blob/master/angularjs-todo/static/services.js
 [src/Site.hs]: https://github.com/nurpax/snap-examples/blob/master/angularjs-todo/src/Site.hs
 [src/Db.hs]: https://github.com/nurpax/snap-examples/blob/master/angularjs-todo/src/Db.hs
