Erlang SQL Migrations
=====================

**Mysql Edition**

This is a very simple utility to add Rails-style migrations to Erlang
projects. Keep in mind that it doesn't have any database abstraction in
it, you'll still operate with plain SQL. Moreso, it only supports
PostgreSQL (using epgsql library) at this moment, but this is fairly
easy to extend if anybody will need that kind of functionality.

Use
---

In order to use the tool one writes 'migration' modules. Most important
thing here is to name in the order of their versioning. Our suggestion
is to use `[timestamp]_name.erl` format. One can generate timestamps
with `date +%s` shell command. Erlang equivalent is
`calendar:datetime_to_gregorian_seconds(
calendar:now_to_universal_time(now()) ) -
calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).`

```erlang
-module(1323220832_add_table).
-export([upgrade/1, downgrade/1]).
-behaviour(sql_migration). %% this line is important

upgrade(C) ->
   pgsql:squery(C, "CREATE TABLE a ()").

downgrade(C) ->
   pgsql:squery(C, "DROP TABLE a").
```

We also suggest to put these files somewhere in `priv` and use this in
rebar.config:

```erlang
{erl_opts, [{src_dirs, ["src","priv/schema"]}]}.
```

This way these migration files will not interfere with your regular
source code.

In your application, this is how you use sqlmig:

```erlang
Migs = sql_migration:migrations(myapp),
sql_migration:migrate(Conn, hd(lists:reverse(Migs)), Migs).
```

You can put it in your `application` module (`myapp_app.erl`), right
after supervisor startup:

```erlang
start(_StartType, _StartArgs) ->
  {ok, Sup = myapp_sup:start_link(),
  init_schema(),
  {ok, Sup}.
```
