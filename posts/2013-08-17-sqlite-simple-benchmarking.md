---
title: Benchmarking sqlite-simple
author: Janne Hellsten
date: August 17, 2013
public: true
---

## Introduction

I recently benchmarked my Haskell SQLite bindings package
[sqlite-simple].  I was curious to know how sqlite-simple performance
compares to native C, Python and other Haskell database bindings.
Initial results for sqlite-simple were extremely poor but improved
significantly after optimizations.  This post will present the results
of this benchmarking.  It also discusses some of the optimizations
that resulted from this performance analysis.

Initially sqlite-simple scored barely over 50K rows/s.  Optimizations
brought this up to 1.8M rows/s, a nice 34x improvement.

## Setup

Here's the setup I used for running my experiments:

* 64-bit Debian running inside VirtualBox (with only a single core enabled)
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
converts the result into a Haskell list of `Int`s and, as a sanity
check, sums the resulting integers together.

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


## Establishing performance targets

My benchmarking goal was to figure out how much overhead the
sqlite-simple library adds on top of raw SQLite performance.  Ideally,
a query should spend all its time in native SQLite and zero time in
Haskell bindings.

To better focus optimization work, I first set out to establish some
reasonable performance targets to compare against.  Establishing
targets was straightforward.  As sqlite-simple runs on top of
direct-sqlite, the sqlite-simple can only be as fast as direct-sqlite.
As direct-sqlite runs on top of the native SQLite library, the fastest
sqlite-simple and direct-sqlite can possibly go is as fast as SQLite.

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


## Results analysis

The collected benchmark data was used to identify various performance
improvement opportunities in sqlite-simple.

Original performance without optimizations was just barely over 50K
rows/s.  This was a performance bug I caused when I forked
sqlite-simple from postgresql-simple.  The problem was in a function
called `stepStmt` that should've been tail recursive but wasn't.
Fixing this in [d2b6f6a5][opt-tail-call] nicely bumped up the score
from 53K to 750K rows/s.

The next couple of optimizations dealt mostly with clean up to reduce
allocation rate.  With [3239d474f0][dofold] and
[0ee050807d][columnlength], the benchmark score went up from 750K to
764K rows/s.

At this point I ran out of low hanging fruit in sqlite-simple and
started to look elsewhere for optimizations.  A low-level
direct-sqlite benchmark was clocking around 2.43M rows/s which seemed
a little low when a C implementation of the same was processing rows
at almost 6.9M rows/s.  Even a Python reimplementation of my benchmark
case was faster at 2.5M rows/s.  To highlight how large the
performance delta between C and direct-sqlite were, it's helpful to
turn the comparison into absolute clock cycles.  On my 3.2GHz machine,
6.9M rows/s means SQLite spends roughly 460 clock cycles per row.

Similarly, at 2.43M rows/s on direct-sqlite, each row cost roughly
1300 clock cycles out of which 460 was spent in the native SQLite
library.  Somehow roughly 840 clock cycles per row were spent in
Haskell SQLite bindings.  The overhead of just calling into SQLite
from Haskell was higher than the actual cost of computing the result
set inside SQLite!  Yet, there wasn't much going on in the wrapper
library.

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

No matter how bad a compiler you might have, there's no way the simple
Haskell code around `sqlite3_step` and `sqlite3_column_int` would add
up to 840 clocks per row.

Turns out FFI call overhead dominated this benchmark.  It was possible
to reduce this overhead by using the `unsafe` FFI calling convention
for some of the SQLite column accessors.  This change was made in
direct-sqlite [pull request
#20](https://github.com/IreneKnapp/direct-sqlite/pull/20).  As the
name `unsafe` implies, it's not OK to simply mark all FFI functions as
unsafe -- please refer to the pull request discussion for details.
The tl;dr of the FFI discussion was that it's OK to mark result data
accessors like `sqlite3_column_int64` and `sqlite3_column_count` as
`unsafe` but that any functions doing the actual query heavy lifting
like `sqlite3_step` should be kept safe.

The FFI optimizations were a big deal: the direct-sqlite benchmark
score went up from 2.43M to 3.6M rows/s.  As expected, this bumped up
the sqlite-simple score up to 1.8M rows/s.

The following chart summarizes how performance progressed through the
above optimizations:

<div>
#### Optimization progress (rows/s)
<div id="chart-opt-progress" style="width:600px;height:360px;"></div>
</div>


## Comparing to other implementations and databases

As an extra bonus, I also compared my results to other databases and
to other Haskell database bindings.  Furthermore, I wanted to know how
well Python fares with its built-in sqlite3 module.  I thus
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
results do seem to confirm though that the HDBC library adds a fairly
significant overhead to database row access.  Comparing HDBC against
sqlite-simple, we get 105K vs 1.8M rows/s.

## Next steps

There are still many areas that would need further analysis:

* Use more columns in the result set
* Use other column types than `Int`s
* Per-query overhead

I expect low per-query overhead to be particularly important for web
applications.  The typical usage in web apps would be that a single
page load performs several queries with relatively low number of rows
returned by each query.

Thanks to [Emmanuel Surleau](https://github.com/Emm), [Irene
Knapp](https://github.com/IreneKnapp) and [Joey
Adams](https://github.com/joeyadams) for their optimization ideas,
contributions and code reviews.

Until next time.

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
           [1808292, "sqlite-simple (optimized)"],
           [53106,   "sqlite-simple (unoptimized)"]
         ]

     var dataProgression = [
         [53106, "Original (w/o optimizations)"],
         [749883, "Tail call optimization"],
         [889363, "Cleanup, strictness, less allocs"],
         [1808292, "With full optimizations (incl. unsafe FFI)"]
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
