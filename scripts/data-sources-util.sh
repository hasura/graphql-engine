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
    curl --fail "$metadata_url" \
    --data-raw '{"type":"pg_add_source","args":{"name":"default","configuration":{"connection_info":{"database_url":"'"$db_url"'"}}}}'
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
    echo "Adding BigQuery source"
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
                  "from_env": "HASURA_BIGQUERY_SERVICE_ACCOUNT"
                },
                "project_id": { "from_env": "HASURA_BIGQUERY_PROJECT_ID" },
                "datasets": ["hasura_test"]
              }
            },
            {
              "name": "hasura_global_limited",
              "kind": "bigquery",
              "tables": [],
              "configuration": {
                "global_select_limit": 1,
                "service_account": {
                  "from_env": "HASURA_BIGQUERY_SERVICE_ACCOUNT"
                },
                "project_id": { "from_env": "HASURA_BIGQUERY_PROJECT_ID" },
                "datasets": ["hasura_test"]
              }
            }
          ]
        }
      }
    }
    '
}

function verify_bigquery_pytest_env() {
    # check that required bigquery environment variables are present
    if [[ -z "${HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE:-}" || -z "${HASURA_BIGQUERY_PROJECT_ID:-}"  ]]; then
        echo "HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE and HASURA_BIGQUERY_PROJECT_ID environment variables are needed to run these tests."
        echo "See https://github.com/hasura/graphql-engine/blob/master/server/CONTRIBUTING.md#running-the-python-test-suite-on-bigquery for more information."
        exit 1
    fi
}
