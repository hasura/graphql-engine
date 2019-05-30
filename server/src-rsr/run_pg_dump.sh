#! /usr/bin/env sh

set -e

filename=/tmp/pg_dump-$(date +%s).sql
template_file=/tmp/hasura_del_lines_template.txt

# input args
DB_URL=$1
OPTS=$2
CLEAN=$3

pg_dump "$DB_URL" $OPTS -f "$filename"

# clean the file the variable is True
if [ "$CLEAN" = "True" ]; then
  # delete all comments
  sed -i '/^--/d' "$filename"

  # delete front matter
  cat > $template_file << EOF
SET xmloption = content;
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
COMMENT ON SCHEMA public IS 'standard public schema';
EOF
  while read -r line; do
    sed -i '/^'"$line"'$/d' "$filename"
  done < $template_file

  # delete notify triggers
  sed -i -E '/^CREATE TRIGGER "?notify_hasura_.+"? AFTER \w+ ON .+ FOR EACH ROW EXECUTE PROCEDURE "?hdb_views"?\."?notify_hasura_.+"?\(\);$/d' "$filename"

  # delete empty lines
  sed -i '/^[[:space:]]*$/d' "$filename"
fi

printf "%s" "$filename"
