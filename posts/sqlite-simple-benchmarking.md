---
title: Benchmarking sqlite-simple
author: Janne Hellsten
date: April 2, 2013
public: false
---

## TOPICS

* TODO:
  - check which is a good single value to report from criterion results (see crit docs)

## Introduction

In this post, I will talk about benchmarking [sqlite-simple] and about
the various optimizations that were made to [sqlite-simple] and
[direct-sqlite].  In addition to SQLite results, I will also compare
my results to other database and other Haskell bindings.  Comparisons
to other database libraries are provided as a kind of sanity check to
validate that my performance numbers are in the right ballpark.

I will not list the usual disclaimers about micro-benchmarks.  You all
know what kind of pitfalls there are to benchmarking.  Comparisons to
code I haven't written will probably be unfair -- I've tuned my code
to do well in my benchmark while I haven't gone through similar
analysis and tuning for other database libraries.

I'd like to thank [Emmanuel Surleau](https://github.com/Emm) for his
optimization ideas and contributions to sqlite-simple.

TODO thank irene and joey too

## Performance targets

My benchmarking goal was to figure out how much overhead does the
sqlite-simple library add on top of raw SQLite performance.  Ideally,
a query should spend all its time in native SQLite and zero time in
Haskell bindings.

I wanted to start with reasonable performance targets.  I find that
it's much easier to focus optimization on relevant things if you
can work against well-defined performance targets.  With good targets,
you will also know when it's time to stop optimizing. :)

Establishing targets was pretty easy.  As sqlite-simple runs on top of
direct-sqlite, the sqlite-simple can only be as fast as direct-sqlite.
As direct-sqlite in turn runs on top of the native SQLite library, the
fastest sqlite-simple and direct-sqlite can possibly go is as fast as
SQLite.

To turn this into numbers, I implemented multiple versions of my query
benchmark (in order of fastest to slowest):

* A native C benchmark on top of the SQLite library ([source](https://github.com/nurpax/db-bench/tree/master/native/main.c))
* Haskell: A Haskell version using direct-sqlite ([source](https://github.com/nurpax/db-bench/tree/master/haskell/Sqlite.hs), see function `selectInts`)
* A Haskell version using sqlite-simple ([source](https://github.com/nurpax/db-bench/tree/master/haskell/Sqlite.hs), see function `selectIntsDS`)

Here's how they perform:

<div>
#### Native C vs Haskell bindings (rows/s)
<div id="chart-c-vs-haskell" style="width:600px;height:360px;"></div>
</div>

Initially sqlite-simple scored barely over 53K rows/s.  After all a
bunch of optimizations the same figure was 1.8M/s, a nice 34x
improvement.

I will go into more details later in this post.  But before that,
let's describe the query benchmark in more detail.

## What was benchmarked

The high-level operation of the benchmark is:

1. Setup: Initialize a table `testdata` with 10000 rows of data
2. Measure the time it takes to query the first column of all these rows.

The schema consists of a single `testdata` table, defined as:

~~~~~{.sql}
CREATE TABLE testdata (id INTEGER PRIMARY KEY, str TEXT, t TIMESTAMP)
~~~~~

See
[db-bench/db-util](https://github.com/nurpax/db-bench/tree/master/db-util)
for more details.

The sqlite-simple query being measured is defined as:

~~~~~{.haskell}
selectInts :: S.Connection -> IO ()
selectInts conn = do
  rows <- S.query_ conn "SELECT id FROM testdata" :: IO [(S.Only Int)]
  checksum $ foldl' (\acc (S.Only v) -> v + acc) 0 rows
~~~~~

Basically, it SELECTs all the rows from the `testdata` table and
converts the result into a a Haskell list of `Int`s.  The resulting
integers are added together as a simple sanity check to see that we
got the right data back.  A faster variant of the same is
`selectIntsFold` which uses `Database.SQLite.Simple.fold` to direcctly
fold over the result rows rather than returning a list of all the
result rows.  Speed advantage is mainly due to less memory being
allocated -- internally both `fold` and `query_` use same data
conversion paths.

Several variants of this function are benchmarked.  The main variants for SQLite are:

* direct-sqlite `selectIntsDS`: Lowest possible API level - least memory allocated
* sqlite-simple `selectIntsFold`: Convert rows into Haskell data but fold over rows to avoid allocating memory for all rows
* sqlite-simple `selectInts`: Convert rows into Haskell list containing all the rows

We'll focus mostly on `selectInts` when comparing against other implementations.

You can find implementations of the same for
various Haskell database libraries under
[db-bench/haskell](https://github.com/nurpax/db-bench/tree/master/haskell).
C and Python implementations are under
[db-bench/native](https://github.com/nurpax/db-bench/tree/master/native)
and
[db-bench/python](https://github.com/nurpax/db-bench/tree/master/python),
respectively.


## Setup

* Debian (64-bit) running inside VirtualBox (with only a single core enabled)
* Quad-core Intel Core i7 at 3.2GHz
* GHC 7.4.2 from the Haskell Platform
* MySQL 5.5.28, PostgreSQL 9.1.7
* Package versions:
    * [direct-sqlite-2.3.3](http://hackage.haskell.org/package/direct-sqlite-2.3.3.1)
    * [sqlite-simple-0.3.0.0](http://hackage.haskell.org/package/sqlite-simple-0.3.0.0)
    * [mysql-simple-0.2.2.4](http://hackage.haskell.org/package/mysql-simple-0.2.2.4)
    * [mysql-0.1.1.4](http://hackage.haskell.org/package/mysql-0.1.1.4)
    * [postgresql-simple-0.2.4.1](http://hackage.haskell.org/package/postgresql-simple-0.2.4.1)
    * [postgresql-libpq-0.8.2.1](http://hackage.haskell.org/package/postgresql-libpq-0.8.2.1)


## Analysis of sqlite-simple results

All this benchmarking would be for nothing if we didn't analyze the
data and take action on it.  With performance targets based on running
native SQLite and direct-sqlite benchmarks, it was easy to identify
several optimizations to sqlite-simple.

Here's a a graph of how sqlite-simple performance progressed over time
as various optimizations were applied:

<div>
#### Optimization progress (rows/s)
<div id="chart-opt-progress" style="width:600px;height:360px;"></div>
</div>

We started off with an abysmal 53K rows/s.  This was an embarrassing
regression I created when I forked sqlite-simple from
postgresql-simple.  The problem was in a function called `stepStmt`
which should've been tail recursive but wasn't.  I suspect it was also
too lazy -- but I never verified this.  Fixing this in
[d2b6f6a5][opt-tail-call] was a major improvement and bumped up the
score from 53K to 750K rows/s.

The next couple of optimizations dealt mostly with clean up that lead
to reduced allocation.  With [3239d474f0][dofold] and
[0ee050807d][columnlength], the benchmark score went up from 750K to
764K rows/sec.

At this point I ran out of low hanging fruit in sqlite-simple and
started to look elsewhere for optimization opportunities.  A low-level
direct-sqlite benchmark was clocking around 2.43M rows/s which seemed
a little when a C implementation of the same was processing rows at
almost 6.9M rows/s.  Even a Python reimplementation of my benchmark
case was faster at 2.5M rows/s.  To highlight how large the
performance delta between C and direct-sqlite were, it's helpful to
turn the comparison into absolute clock cycles.  On my 3.2GHz machine,
6.9M rows/s means SQLite spends roughly 460 clock cycles per row.

Similarly, at 2.43M rows/s on direct-sqlite, each row cost roughly
1300 clock cycles out of which 460 was spent in the native SQLite
library.  Somehow roughly 840 clock cycles per row were spent in the
SQLite Haskell bindings.  The overhead in calling into SQLite was
higher than the actual cost of returning the query results!  Yet,
there wasn't much going on in the wrapper library.

Consider the innerloop of the direct-sqlite benchmark:

~~~~~{.haskell}
sumRowsColumnInt64 :: DS.Statement -> Int64 -> IO Int64
sumRowsColumnInt64 stmt !acc = do
  r <- DS.step stmt
  case r of
    DS.Row ->
      do
        i <- DS.columnInt64 stmt 0
        sumRowsColumnInt64 stmt (acc + i)
    DS.Done ->
      return acc
~~~~~

The direct-sqlite calls `DS.step` and `DS.columnInt64` map quite
directly to their native SQLite counterparts
<code>[sqlite3_step](http://www.sqlite.org/c3ref/step.html)</code> and
<code>[sqlite3_column_int](http://www.sqlite.org/c3ref/column_blob.html)</code>.
Thus the expectation is that their cost should be roughly the same as
in the C version of this benchmark.

No matter how bad a compiler you might have, there's no way that the
simple Haskell code around `sqlite3_step` and `sqlite3_column_int`
would take 840 clocks per row.

Turns out it's the FFI call overhead that dominated this benchmark.
In our case it was possible to reduce this overhead by using the
`unsafe` FFI calling convention for some of the SQLite column
accessors.  This change was made in direct-sqlite [pull request
#20](https://github.com/IreneKnapp/direct-sqlite/pull/20).  As the
name `unsafe` implies, it's not OK to simply mark all FFI functions as
unsafe -- please refer to the pull request discussion for details.
The tl;dr of the FFI discussion was that it's OK to mark result data
accessors like `sqlite3_column_int64` and `sqlite3_column_count` as
`unsafe` but that any functions doing the actual query heavy lifting
like `sqlite3_step` should be kept safe.

The FFI optimizations were a big deal: the direct-sqlite benchmark
score went up from 2.43M to 3.6M rows/s.  As expected, this in turn
bumped up the sqlite-simple score up by roughly 1M rows/s.

## Comparing to other implementations and databases

I thought it'd be interesting to compare my results to other databases
and also other Haskell database bindings.  Furthermore, I wanted to
know how fast Python fares with its built-in sqlite3 module.  So I
implemented a few more variants of my benchmark:

* mysql-simple ([source](https://github.com/nurpax/db-bench/tree/master/haskell/Mysql.hs))
* postgresql-simple ([source](https://github.com/nurpax/db-bench/tree/master/haskell/Psql.hs))
* HDBC-sqlite3 ([source](https://github.com/nurpax/db-bench/tree/master/haskell/Hdbc.hs))
* Python sqlite3 ([source](https://github.com/nurpax/db-bench/tree/master/python/bench.py))

<div>
#### Database/library comparison (rows/s)
<div id="chart-cross-db" style="width:600px;height:360px;"></div>
</div>

I didn't do any performance analysis for non-sqlite benchmark results,
and wouldn't draw too many conclusions about the results.  These
results do seem to confirm though that the HDBC library seems to add
fairly significant overhead to database access.  Comparing HDBC
against sqlite-simple, we get 105K vs 1.8M rows/sec.

## Conclusion

TODO:

* Lessons learned?
* Next steps - e.g., benchmark other data conversions like dates which are known to be slow



<script type="text/javascript">
function barchart(divname, legendPos, data)
{
    var i = 0;
    var d1 = data.map(function(elt) { return { label: elt[1], data: [[i++, elt[0]]]}; });
/*
    i = 0;
    var xaxisLabels = data.map(function(elt) { return [i++, elt[1]]; });
*/

    var options = {
         series: {
             bars: {
                 show: true,
                 lineWidth:0,
                 fill:true,
                 barWidth: 0.9,
                 fillColor: { colors: [ { opacity: 1.0 }, { opacity: 1.0 } ] },
                 align: 'center'
             },
         },
         legend: { position: legendPos, },
         xaxis: { min:-1, max:i, ticks: [] },
     };

    $.plot($(divname), d1, options);
}

$(function () {
     var data = [
         [6891798, "native sqlite (C)"],
         [3600395, "direct-sqlite"],
         [2208862, "sqlite-simple (fold_)"],
         [1808292, "sqlite-simple (query_)"],
         [2500000, "Python"],
         [944227,  "mysql-simple"],
         [466932,  "postgresql-simple"],
         [105529,  "HDBC-sqlite3"]
         ];


     var dataNativeVsHaskell = [
           [6891798, "Native C"],
           [3600395, "direct-sqlite"],
           [1808292, "sqlite-simple"]
         ]

     var dataProgression = [
         [53106, "Original"],
         [749883, "Tail call optimization"],
         [889363, "Cleanup, strictness, less allocs"],
         [1808292, "Unsafe flag"]
         ]

     barchart("#chart-cross-db", "ne", data);
     barchart("#chart-c-vs-haskell", "ne", dataNativeVsHaskell);
     barchart("#chart-opt-progress", "nw", dataProgression);
});
</script>


 [sqlite-simple]: https://github.com/nurpax/sqlite-simple
 [direct-sqlite]: https://github.com/IreneKnapp/direct-sqlite
 [opt-tail-call]: https://github.com/nurpax/sqlite-simple/commit/d2b6f6a50e116e1bad28395eac4646c1e6b34c4b
 [dofold]: https://github.com/nurpax/sqlite-simple/commit/3239d474f033b2ab048744c9ea177b1e36930cce
 [columnlength]: https://github.com/nurpax/sqlite-simple/commit/0ee050807ded66b2eb34238f2a8d91cde6be8aa1
 [ffi-unsafe]: https://github.com/IreneKnapp/direct-sqlite/commit/309536f84f095301a4ce675ae54140d492adb827
