---
title: React Redux Todo app running on a Haskell server
author: Janne Hellsten
date: November 1, 2016
public: true
---

I put together a single-page web app (SPA) todo example for use in my personal projects.  Its most noteworthy feature is end-to-end authentication implemented using [redux-auth-wrapper] and [JWT].  The project implements a simple todo application with user account support.  Data is persisted on a server implemented in Haskell.  It's not super complex but it took quite a bit of reading to put all the pieces together, so I figured I might as well publish it for others to learn from.

You can find the application source code [on github][example].

The project uses the following tools, libraries and techniques:

- [React][reactjs], [Redux][redux] and [React-redux][react-redux]
- [Webpack][webpack] and [Babel][babel] (for JSX, ES6+ and CSS modules)
- [Redux-thunk][redux-thunk] for async fetching in Redux actions
- [React-router][react-router] and [React-router-redux][react-router-redux] for routing
- [Redux-auth-wrapper][redux-auth-wrapper] for implementing robust authenticated routing and login screens
- [JWT][jwt]-based authentication that works well with Redux and routing
- Haskell for the backend using the [Snap web framework][snap] to serve the REST API
- Roll-my-own JWT authentication code for the Haskell backend

The development setup does not implement hot module reloading (HMR) or server-side rendering (SSR).  I imagine it's possible to extend this setup for HMR.  I don't know if SSR is feasible without a Node.js based server.

I won't explain each and every one of the above libraries in details.  They all have fairly good documentation and a whole bunch of tutorials on the net.  Instead, I will just highlight some of the most relevant parts.

# Client-side

Client routes are setup in [client/containers/Root.js](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/client/containers/Root.js):

```
const Root = () => (
  <Provider store={store}>
    <Router history={history}>
      <Route path='/' component={Main} />
      <Route path='/todos' component={UserIsAuthenticated(TodoList)} />
      <Route path='/login' component={Login} />
      <Route path='/profile' component={UserIsAuthenticated(Profile)} />
      <Route path='/signup' component={SignUp} />
    </Router>
  </Provider>)
```

This sets up various routes like the home screen, todo list and login and logout routes.  Notice the `UserIsAuthenticated` higher-order component that applies authentication constraints on the `TodoList` and `Profile` components.  Those routes won't be accessible unless the user is logged in.

See [redux-auth-wrapper] for more information on setting up authentication and redux.

## Authenticated fetch

Client-side JWT handling is implemented in [client/auth.js](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/client/auth.js).  It implements Redux actions and reducers related to user authentication.  It exports a function called `fetchWithAuth` which is used instead of `fetch` whenever accessing API endpoints that require authentication.

Here's an example of how `fetchWithAuth` is used in [client/actions.js](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/client/actions.js) to save a todo item:

```
export function saveTodo (todo) {
  return function (dispatch, getState) {
    return fetchWithAuth(getState, '/api/todo', { method: 'POST', body: todo })
      .then(json => dispatch(receiveTodo(json)))
  }
}
```

The `fetchWithAuth` function is basically just a wrapper around `fetch` that inserts the JWT user token into the Authorization header when issuing a fetch call.

It currently does not handle expired tokens, though.  It should be easy to extend it such that it will detect an expired token error, and issue an action that triggers a client-side redirect to the login screen.

# API server

The backend is written in Haskell.  It uses the Snap framework to get a basic web server running.  It's main functionality is to persist users and todo items and serve them via a JSON API.  Apart from the one static [index.html](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/static/index.html) used to bootstrap React, the server does not render any HTML.

The relevant source files are:

- [src/Site.hs](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/src/Site.hs) -- Routes and API implementation
- [src/Db.hs](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/src/Db.hs) -- [sqlite-simple] code for persisting todo items into a SQLite3 database
- [src/Snaplet/SqliteJwt.hs](https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/src/Snap/Snaplet/SqliteJwt.hs) -- A Snap middleware that implements user accounts, password hashing and salting using BCrypt, and JWT for sessionless authentication.

The request handler for todo items looks like so:

```{.haskell}
handleRestTodos :: H ()
handleRestTodos = (method GET listTodos) <|> (method POST saveTodo)
  where
    listTodos :: H ()
    listTodos = replyJson query
      where
        query (J.User uid _) = withTop db $ Db.listTodos (Db.UserId uid)

    saveTodo = do
      ps <- reqJSON
      replyJson (query ps)
      where
        query ps (J.User uid _) =
          -- If the input todo id is Nothing, create a new todo.  Otherwise update
          -- an existing one.
          case ptId ps of
            Nothing -> do
              withTop db $ Db.newTodo (Db.UserId uid) (ptText ps)
            Just tid -> do
              let newTodo = Db.Todo tid (ptSavedOn ps) (ptCompleted ps) (ptText ps)
              withTop db $ Db.saveTodo (Db.UserId uid) newTodo

    replyJson :: ToJSON a => (J.User -> Handler App J.SqliteJwt a) -> H ()
    replyJson action = do
      res <- with jwt $ J.requireAuth action
      writeJSON res
```

Query parameters for the `saveTodo` POST requests are passed in as JSON and parsed with the [aeson] JSON parser.  Static type checking enforces that invalid request parameters don't get past the JSON parser.

Server-side authentication is implemented in the [SqliteJwt] module.  It exports the `requireAuth` function which is used above to wrap request handlers.  A wrapped handler will either be passed down the currently authenticated user or the request is terminated if JWT authentication failed.

# Future work

The Haskell SqliteJwt module is work-in-progress and certainly not ready for production.  For example, it doesn't currently support token expiration or revoking existing tokens.  It also hardcodes the site secret used to sign JWTs.  I am planning to develop this module further to support these features and maybe publish it on Hackage.

It'd also be interesting to plug in an external authentication mechanism like Google OAuth 2.0.  I haven't found good examples on how to do this robustly.  There is discussion about this in redux-auth-wrapper [github issue #46](https://github.com/mjrussell/redux-auth-wrapper/issues/46) but I haven't tried toying around with those ideas yet.  It will probably be quite a bit trickier than local authentication.

Thanks for reading!

[example]: https://github.com/nurpax/snap-reactjs-todo/tree/blog-0.1
[reactjs]: https://facebook.github.io/react/
[redux]: https://github.com/reactjs/redux
[react-redux]: https://github.com/reactjs/react-redux
[webpack]: https://webpack.github.io/
[babel]: https://babeljs.io/
[redux-thunk]: https://github.com/gaearon/redux-thunk
[react-router]: https://github.com/ReactTraining/react-router
[react-router-redux]: https://github.com/reactjs/react-router-redux
[redux-auth-wrapper]: https://github.com/mjrussell/redux-auth-wrapper
[jwt]: https://jwt.io/
[snap]: http://snapframework.com/
[aeson]: https://hackage.haskell.org/package/aeson
[SqliteJwt]: https://github.com/nurpax/snap-reactjs-todo/blob/blog-0.1/src/Snap/Snaplet/SqliteJwt.hs
[sqlite-simple]: https://hackage.haskell.org/package/aeson
