# Introduction

So, getting started, you'll need to get your SQLite database
setup. Presuming you have SQLite3 installed and ready to go, we'll just
go to `gambletron.migrations.lahman` namespace and in a repl run
`(migrate)`. This will import all the relevant information.

For the source of statistics, we will use Lahman's [data](http://www.seanlahman.com/baseball-archive/statistics/).
Just download it. Get the CSV, the SQL dump is optimized for MySQL and
won't work for anything else.

We'll use SQLite for the database, because we're not doing anything
"serious". Unfortunately, as noted, the SQL dump is optimized for
MySQL...and won't work for SQLite (there are 69 errors for SQLite
3.8.7.4 if you just try `.read lahman2014_beta.sql`).

There are five main tables: `player` (corresponding to Lahman's
`MASTER`), `batting`, `pitching`, `fielding`, and `team`. The sordid
details are handled elsewhere.

## Instructions

First, go to `gambletron/resources/` and open a terminal:

```
alex@avocado:~/gambletron/resources$ sqlite3 baseball.db
SQLite version 3.8.7.4 2014-12-09 01:34:36
Enter ".help" for usage hints.
sqlite> .read schema.sql

sqlite> .quit
```

Then open up a repl:

```clojure
alex@avocado:~/gambletron/resources$ cd ..
alex@avocado:~/gambletron$ lein repl
nREPL server started on port 33937 on host 127.0.0.1 - nrepl://127.0.0.1:33937
REPL-y 0.3.5, nREPL 0.2.10
Clojure 1.6.0
OpenJDK 64-Bit Server VM 1.8.0_45-internal-b14
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (require 'gambletron.migrations.lahman)
Jun 20, 2015 12:39:19 PM com.mchange.v2.log.MLog <clinit>
INFO: MLog clients using java 1.4+ standard logging.
nil
user=> (ns gambletron.migrations.lahman)
nil
gambletron.migrations.lahman=> (migrate)
;; prints out progress
:done
gambletron.migrations.lahman=> (quit)
Bye for now!
alex@avocado:~/gambletron$ 
```

## Performance notes

For speed reasons, we import the data as a number of single transactions
(one transaction per table). This speeds the migration up by several
orders of magnitude.

Other optimizations include `PRAGMA synchronous = OFF`, although this
may not be necessary on modern systems. If you are using an older
system, you may want to examine this
[StackOverflow thread](http://stackoverflow.com/q/1711631) for
optimizing SQLite insertions.

For additional speed, you could try commenting out the `transform`
components of each `(defentity ...)` in `sabermetrics.schema`.

Locally, on a quad-core 2.8GHz i5-4440S it took 93 seconds (92573.446679
msecs) to import the data "as is". When the transform statements did not
include the `map->record` lines (the `map->Team`, `map->Pitching`,
etc.), it took 83 seconds (83503.261744 msecs) to populate the entire
database (332,478 rows) without indices. With indices, it takes the same
amount of time.

Your mileage may vary...
