---
title: REST API testing in Haskell with wreq and test-framework
author: Janne Hellsten
date: May 4, 2014
public: true
---

## Introduction

This blog post is about my experiences rewriting
Node.js/JavaScript-based REST unit tests in Haskell using [wreq] and
[test-framework].  I hope the test cases presented in this post are
useful practical examples of using `test-framework` for IO-heavy
testing.

The tests discussed in this post are for my [Hswtrack][hswtrack]
project.  Hswtrack is a web application for exercise tracking.  The
application server is built with [Snap][snap] and the UI in JavaScript
with JQuery and [Handlebars.js][handlebarsjs].

Here's a summary of what type of REST entry points will be tested in
this post:

| Entry point      | Verb | Description                                               |
| -----------      | ---- | -----------                                               |
| `/rest/login`    | POST | Login with `login` and `password` parameters.             |
| `/rest/new_user` | POST | Create a user with `login` and `password` parameters.     |
| `/rest/app`      | GET  | Get logged in user login name and status.                 |
| `/rest/exercise` | GET  | List existing exercises.                                  |
| `/rest/exercise` | POST | Create a new exercise  with `name` and `type` parameters. |

Most entry point require a prior successful login.  The response body
is encoded as JSON.  When called unauthenticated, the server returns
an HTTP 403 error code.

As the server talks to the client in JSON, I initially thought
JavaScript + NodeJS would be a nice combo for developing these tests.
I first tried [Frisby](http://frisbyjs.com/) which looked simple and
had good looking documentation.  Unfortunately it turned out to be
cumbersome for the types of tests I wanted to write.  I also tried
writing my own test framework with Q promises but that came out like
[Tea party code][teapartycode] too. (You can view the source for my
JavaScript tests [here][jstestexample].)

Feeling dissatisfied about the state of my tests, I decided to rewrite
my unit tests in Haskell.

## Types of tests

I wanted to develop the following types of tests:

  * Test that user creation works
  * Test that login works
  * Test that REST entry points deny access if not logged in
  * Test object creation, update and deletion when logged in
  * Test that attempts to modify or delete other users' data are rejected

Any tests that modify or create objects need to run with an
authenticated user.  This means each such test would need to either
log in as part of its init sequence or have its authentication cookies
passed into it.  I went for the latter approach as it allows logging
in once and running multiple tests with the same cookies.

I define top-level test cases called `createUserTests` and
`loginUserTests` which first perform user creation or login, followed
by running a list of sub-tests with cookies acquired from the login
process.  The sequencing is explicitly declared by using
`test-framework`'s `TestGroup`s and `Test`s.

Here's the definition of `loginUserTests`:

```{.haskell}
-- | Login a user and run a list of subtests with cookies acquired
-- from the login process.
loginUserTests :: [(String, Options -> Assertion)] -> IO Test
loginUserTests tests = do
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  return
    . testGroup "Tests with logged in user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

```

It performs a HTTP POST on `/rest/login` with the test user's login
credentials, grabs the authentication cookies with `defaults & cookies
.~ (r ^. responseCookieJar)` and passes them to sub-tests so that they
can run authenticated.

Here's how `loginUserTests` gets used:

```{.haskell}
-- Test case that tests that we're successfully logged in
-- Options must contain the necessary login cookies.
testLoggedInOk :: Options -> Assertion
testLoggedInOk opts = do
  r <- getWith opts (mkUrl "/rest/app")
  Just True @=? (r ^? responseBody . key "loggedIn" . _Bool)

main :: IO ()
main =
  defaultMain
  [ buildTest $ createUserTests [("logged in?", testLoggedInOk)]
  , buildTest $ loginUserTests  [("logged in?", testLoggedInOk)]
  ]
```

Function `testLoggedInOk` tests that it can HTTP GET `/rest/app`
successfully.  It also tests that the returned JSON object contains a
field `loggedIn` with value `true`.

## Negative testing

One shouldn't forget about negative testing, so let's develop a test
for checking that entry points correctly respond with an error 403 on
unauthenticated access:

```{.haskell}
-- GET requests against 'url' and expect to get error 403 back
testLoggedInFail :: String -> Options -> Assertion
testLoggedInFail url opts = do
  E.try (getWith opts url) >>= check
  where
    check (Left (HT.StatusCodeException s _ _))
      | s ^. statusCode == 403 = assertBool "error ok" True
      | otherwise              = assertBool "unexpected status code" False
    check (Left _)  = assertBool "unexpected exception caught" False
    check (Right r) = assertBool "req should've failed" False

main :: IO ()
main =
  defaultMain [testGroup "Require auth fail" requireAuthFail]
  where
    requireAuthFail =
      map (\u -> testCase u (testLoggedInFail (mkUrl u) defaults)) authReqd
    -- REST entry points which require user to be logged in
    authReqd = [ "/rest/app"
               , "/rest/weights"
               , "/rest/notes"
               , "/rest/exercise"
               , "/rest/workout/exercise"
               , "/rest/workout"
               , "/rest/workout"
               , "/rest/stats/workout"
               ]
```

## Creating objects

Here we will test two entry points: one for creating a new exercise
like "Chin-ups" and one for listing existing exercises.  Creation is a
HTTP POST to `/rest/exercise` and listing exercises is HTTP GET
`/rest/exercise`.

A successful POST to `/rest/exercise` will create a new object on the
server and return the object to the client as JSON.  Here's what that
repsonse might look like:


```
{ "id":2, "name": "Chin-ups", "type":"BW" }
```

A successful GET of `/rest/exercise` will retrieve the full list of
available exercises.  It looks something like this:

```
[
  { "id":1, "name": "Push-ups", "type":"BW" },
  { "id":2, "name": "Chin-ups", "type":"BW" }
]
```

To test creating exercise objects, we'll create an exercise, verify
that its returned properties match what we gave on creation, and
finally retrieve the complete exercise list and check that the object
is listed.

Here's the code for the above test strategy.  The [Aeson Lens
API][aeson-lens] is particularly handy for this type of ad hoc JSON
value inspection!


```{.haskell}
testAddExercise :: Options -> Assertion
testAddExercise opts = do
  let name = "Chin-ups" :: T.Text
      ty   = "BW"       :: T.Text
  r <- postWith opts (mkUrl "/rest/exercise") ["name" := name, "type" := ty]
  -- Verify that the newly created object matches creation params
  name @=? r ^. responseBody . key "name" . _String
  ty   @=? r ^. responseBody . key "type" . _String
  -- Verify that the object ended up in the global list of exercises
  let (Just oid) = r ^? responseBody . key "id" . _Integer
  r <- getWith opts (mkUrl "/rest/exercise")
  assertBool "oid should be in list" (oid `elem` exercises r)
  where
    exercises r = r ^.. responseBody . values . key "id" . _Integer
```

## Conclusion

I only just got started developing these tests and so coverage is
still poor.  I'm pretty sure that as I develop new tests, interesting
ways to restructure and generalize these tests will emerge.

You can find the full source code to these tests [here][testsrc].

Thanks for reading!

 [snap]: http://snapframework.com/
 [hswtrack]: https://github.com/nurpax/hswtrack
 [handlebarsjs]: http://handlebarsjs.com/
 [jstestexample]: https://github.com/nurpax/hswtrack/blob/0e820183ce28a6e62056c0dda7c99d5109ad3e68/test/rest/tests/workout.js
 [teapartycode]: https://twitter.com/SlexAxton/status/455568049181958144
 [wreq]: http://hackage.haskell.org/package/wreq
 [test-framework]: https://batterseapower.github.io/test-framework/
 [testsrc]: https://github.com/nurpax/hswtrack/blob/blog-may-version/test/Test.hs
 [aeson-lens]: http://hackage.haskell.org/package/lens/docs/Data-Aeson-Lens.html
