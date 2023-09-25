# Hasura Frontend Docker Setup

The `docker-compose.template.yml` file in this directory is a starter template `docker-compose.yml` file to create several local dbs, and relevant services for local Hasura frontend development. In addition, the aim for this will be to automatically populate db's with demo data as part of the initialization process.

### There are a few basic steps:

Before beginning, create a `docker-compose.yml` file in this directory and copy and paste the contents of `docker-compose.template.yml` into this file. Since the `docker-compose.yml` is not part of source control, you may freely alter it to fit your needs.

1. Run `docker compose up -d` from this directory.
2. Enter the BigQuery service account private key for the BigQuery source. Property is, `configuration.service_account.private_key` in the `default_metadata.json` file. Alternatively, you can remove this section of the json file.
3. Go to Settings > Metadata Actions -- [or just click here](http://localhost:8080/console/settings/metadata-actions) (you will need to enter your admin secret: `moocow`)
4. Select `Import Metadata` and select `default_metadata.json`

**Note:** By default, no tables are tracked. This is intentional as the Chinook database is being used for most of these and things can get a bit confusing if they are all tracked at once. I would recommend either using namespaces, or just track as needed.

## BigQuery

If you would like to have a working BigQuery database first obtain the BigQuery service account key JSON data (if you don't know, then ask). Then, enter the private key in your `default_metadata.json`.

You may also need to flip the [feature flags here](http://localhost:4200/settings/feature-flags) to see everything in your local instance correctly.

## MSSQL / SQL Server

In order to initialize and populate a MSSQL demo database automatically, we making use of the `mssql-init` container. This image runs a few commands against the `mssql` image to create the demo data.

This scripting is done in `docker/DataSources/mssql/run-initialization.sh`.

After `run-initialization.sh` attempts to execute `docker/DataSources/mssql/mssql-setup.sql`, it creates the file `docker/DataSources/mssql/REMOVE_ME_TO_RERUN_SETUP.txt`. The presence of this `.txt` file will prevent the `mssql-setup.sql` script from executing again if the container is started again.

To force it to re-run, just delete `/DataSources/mssql/REMOVE_ME_TO_RERUN_SETUP.txt` and restart your container. Because `/DataSources/mssql` is mounted as a volume, it uses the local filesystem.

## MongoDB

The Docker compose file starts a Mongo database with a single collection called `mycollection`. This collection is defined with a json schema file, since for the moment the Mongo agent can only track collections with a schema.

### Database

The docker-compose file will start a Mongo database. You can connect to it with the following command:

```bash
mongosh "mongodb://host.docker.internal:27017"
```

For now the agent only works with databases without authentication, so keep that in mind if you're planning to use a different Mongo database than the one provided in `docker-compose.yml`

### Agent

For the moment we don't have a docker image for the Mongo agent, but you can start it locally with the following steps:

- Install cabal (Haskell build tools)
- Go to the `pro/dc-agents/mongodb` directory
- `cabal build`
- `cabal update`
- `cabal run`
- Add an agent. Set URL to `http://host.docker.internal:8888`. Replace with `localhost` if your OS does not support `host.docker.internal`
- Add a Mongo database
  - Set URL to `host.docker.internal:27017`
  - Set DB to `sample`

## Using Different HGE Images

You may need to replace the `image:` in the compose file with a different image. You can do this by downloading an image, and running `docker load -i <the new image>`. Once it's loaded copy the image name into the compose. Here's an example using a CI built image:

```yaml
image: hasura/graphql-engine:dev-e3bc433-main.ubuntu.arm64
```
