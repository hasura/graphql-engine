# Data Connector Agent Reference Implementation

This directory contains a barebones implementation of the Data Connector agent specification which fetches its data from static JSON files. It can be used as a reference implementation for testing, and as a reference for developers working on backend services.

## Requirements
* NodeJS 16

## Build & Run
```
> npm install
> npm start
```

## Docker Build & Run
```
> docker build . -t dc-reference-agent:latest
> docker run -it --rm -p 8100:8100 dc-reference-agent:latest
```

## Dataset
The dataset exposed by the reference agent is sourced from https://github.com/lerocha/chinook-database/

More specifically, the `Chinook.xml.gz` file is a GZipped version of https://raw.githubusercontent.com/lerocha/chinook-database/ce27c48d9f375f81b7b68bacdfddf3c4458acc49/ChinookDatabase/DataSources/_Xml/ChinookData.xml

The `schema-tables.json` is manually derived from the schema of the data as can be seen from the `CREATE TABLE` etc DML statements in the various per-database-vendor SQL scripts that can be found in `/ChinookDatabase/DataSources` in that repo.

The datasets can be operated on via the `/datasets` resources as described in `dc-agents/README.md`.

## Configuration
The reference agent supports some configuration properties that can be set via the `value` property of `configuration` on a source in Hasura metadata. The configuration is passed to the agent on each request via the `X-Hasura-DataConnector-Config` header.

The configuration that the reference agent can take supports two properties:

* `tables`: This is a list of table names that should be exposed by the agent. If omitted all Chinook dataset tables are exposed. If specified, it filters all available table names by the specified list.
* `schema`: If specified, this places all the tables within a schema of the specified name. For example, if `schema` is set to `my_schema`, all table names will be namespaced under `my_schema`, for example `["my_schema","Album"]`. If not specified, then tables are not namespaced, for example `["Album"]`.

Here's an example configuration that only exposes the Artist and Album tables, and namespaces them under `my_schema`:

```json
{
  "tables": ["Artist", "Album"],
  "schema": "my_schema"
}
```

Here's an example configuration that exposes all tables, un-namespaced:

```json
{}
```
