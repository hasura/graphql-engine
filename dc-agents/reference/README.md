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

# Dataset
The dataset exposed by the reference agent is sourced from https://github.com/lerocha/chinook-database/

More specifically, the `ChinookData.xml.gz` file is a GZipped version of https://raw.githubusercontent.com/lerocha/chinook-database/ce27c48d9f375f81b7b68bacdfddf3c4458acc49/ChinookDatabase/DataSources/_Xml/ChinookData.xml

The `schema-tables.json` is manually derived from the schema of the data as can be seen from the `CREATE TABLE` etc DML statements in the various per-database-vendor SQL scripts that can be found in `/ChinookDatabase/DataSources` in that repo.
