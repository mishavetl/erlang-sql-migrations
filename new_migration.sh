## Usage: new_migration.sh create_foo {name_of_app}

name=$(date +%s)'_'$1

mkdir -p apps/$2/src/migrations

echo '-module('\'$name\'').
-export([upgrade/1, downgrade/1]).
-behaviour(sql_migration).

upgrade(Pid) ->
    mysql:query(Pid, "CREATE TABLE foo ()").

downgrade(Pid) ->
    mysql:query(Pid, "DROP TABLE foo").
' > apps/$2/src/migrations/$name.erl
