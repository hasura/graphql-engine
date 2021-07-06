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
            add_mssql_source "$hasura_graphql_server_port" "$MSSQL_DB_URL"
        ;;
        # bigquery deliberately omitted as its test setup is atypical. See:
        # https://github.com/hasura/graphql-engine/blob/master/server/CONTRIBUTING.md#running-the-python-test-suite-on-bigquery
    esac

    echo ""
}

function add_postgres_source() {
    hasura_graphql_server_port=${1}
    db_url=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding Postgres source"
    curl "$metadata_url" \
    --data-raw '{"type":"pg_add_source","args":{"name":"default","configuration":{"connection_info":{"database_url":"'"$db_url"'"}}}}'
}

function add_citus_source() {
    hasura_graphql_server_port=${1}
    db_url=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding Citus source"
    curl "$metadata_url" \
    --data-raw '{"type":"citus_add_source","args":{"name":"citus","configuration":{"connection_info":{"database_url":"'"$db_url"'"}}}}'
}

function add_mssql_source() {
    hasura_graphql_server_port=${1}
    connection_string=${2}
    metadata_url=http://127.0.0.1:$hasura_graphql_server_port/v1/metadata

    echo ""
    echo "Adding SQL Server source"
    curl "$metadata_url" \
    --data-raw '{"type":"mssql_add_source","args":{"name":"mssql","configuration":{"connection_info":{"connection_string":"'"$connection_string"'"}}}}'
}
