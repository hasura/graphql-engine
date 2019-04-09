#! /usr/bin/env sh

set -e

filename=/tmp/pg_dump-$(date +%s).sql
template_file=/tmp/hasura_del_lines_template.txt

# input args
DB_URL=$1
SCHEMA=$2

pg_dump -O -x "$DB_URL" --schema-only --schema "$SCHEMA" -f "$filename"

# delete all comments

sed -i '/^--/d' "$filename"

# delete front matter

cat > $template_file << EOF
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;
SET default_tablespace = '';
SET default_with_oids = false;
CREATE SCHEMA public;
EOF

while read -r line; do
    sed -i '/^'"$line"'$/d' "$filename"
done < $template_file

# delete notify triggers

sed -i -E '/^CREATE TRIGGER "?notify_hasura_\w+"? AFTER \w+ ON "?\w+"?\."?\w+"? FOR EACH ROW EXECUTE PROCEDURE "?hdb_views"?\."?notify_hasura_\w+"?\(\);$/d' "$filename"

# delete empty lines

sed -i '/^[[:space:]]*$/d' "$filename"

echo "$filename"
