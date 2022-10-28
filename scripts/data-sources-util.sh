#!/usr/bin/env bash
# functions common to dev.sh (local testing) and test-server.sh (CI tests)
function add_sources() {
    hasura_graphql_server_port=${1:-8181}

    # always add a Postgres source for metadata, at least
    add_postgres_source "$hasura_graphql_server_port" "$PG_DB_URL"

    case "$BACKEND" in
        citus)
            add_citus_source "$hasura_graphql_server_port" "$CITUS_DB_URL"
        ;;
        mssql)
            add_mssql_source "$hasura_graphql_server_port" "$MSSQL_CONN_STR"
        ;;
        mysql)
            add_mysql_source "$hasura_graphql_server_port"
        ;;
        bigquery)
            add_bigquery_source "$hasura_graphql_server_port"
        ;;
    esac

    echo ""
}

function add_postgres_source() {
    hasura_graphql_server_port=${1}
    db_url=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding Postgres source"
    # FIXME: Without a loop here `dev.sh test` fails for me here. With the loop
    # I get “source with name \"default\" already exists”
    until curl -s "$metadata_url" \
        --data-raw '{"type":"pg_add_source","args":{"name":"default","configuration":{"connection_info":{"database_url":"'"$db_url"'"}}}}'; &>/dev/null; do
      echo -n '.' && sleep 0.2
    done
    echo " Ok"
}

function add_citus_source() {
    hasura_graphql_server_port=${1}
    db_url=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding Citus source"
    curl --fail "$metadata_url" \
    --data-raw '{"type":"citus_add_source","args":{"name":"citus","configuration":{"connection_info":{"database_url":"'"$db_url"'"}}}}'
}

function add_mssql_source() {
    hasura_graphql_server_port=${1}
    connection_string=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding SQL Server source"
    curl --fail "$metadata_url" \
    --data-raw '{"type":"mssql_add_source","args":{"name":"mssql","configuration":{"connection_info":{"connection_string":"'"$connection_string"'"}}}}'
}

function add_mysql_source() {
    hasura_graphql_server_port=${1}
    # connection_string currently unused as mysql_add_source not yet supported
    # connection_string=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding MySQL source"
    curl --fail "$metadata_url" \
    --data-raw '{"type":"replace_metadata","args":{"version":3,"sources":[{"name":"mysql","kind":"mysql","tables":[],"configuration":{"database":"hasura","user":"'"$MYSQL_USER"'","password":"'"$MYSQL_PASSWORD"'","host":"127.0.0.1","port":'"$MYSQL_PORT"',"pool_settings":{}}}]}}'
}

function add_bigquery_source() {
    hasura_graphql_server_port=${1}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding BigQuery sources to project $HASURA_BIGQUERY_PROJECT_ID"
    curl --fail "$metadata_url" \
    --data-raw '
    {
      "type": "replace_metadata",
      "args": {
        "metadata": {
          "version": 3,
          "sources": [
            {
              "name": "bigquery",
              "kind": "bigquery",
              "tables": [],
              "configuration": {
                "service_account": {
                  "from_env": "HASURA_BIGQUERY_SERVICE_KEY"
                },
                "project_id": { "from_env": "HASURA_BIGQUERY_PROJECT_ID" },
                "datasets": ["hasura"]
              }
            },
            {
              "name": "bigquery2",
              "kind": "bigquery",
              "tables": [],
              "configuration": {
                "service_account": {
                  "from_env": "HASURA_BIGQUERY_SERVICE_KEY"
                },
                "project_id": { "from_env": "HASURA_BIGQUERY_PROJECT_ID" },
                "datasets": ["hasura"]
              }
            },
            {
              "name": "hasura_global_limited",
              "kind": "bigquery",
              "tables": [],
              "configuration": {
                "global_select_limit": 1,
                "service_account": {
                  "from_env": "HASURA_BIGQUERY_SERVICE_KEY"
                },
                "project_id": { "from_env": "HASURA_BIGQUERY_PROJECT_ID" },
                "datasets": ["hasura"]
              }
            }
          ]
        }
      }
    }
    '
}
