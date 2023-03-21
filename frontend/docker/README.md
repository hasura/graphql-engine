# Hasura Frontend Docker Setup

The `docker-compose.yml` file in this directory is intended to create several local dbs, and relevant services for local Hasura frontend development. In addition, the aim for this will be to automatically populate db's with demo data as part of the initialization process.

There are a few basic steps:

1. Run `docker compose up -d` from this directory.
2. Enter the BigQuery service account private key for the BigQuery source. Property is, `configuration.service_account.private_key` in the `default_metadata.json` file. Alternatively, you can remove this section of the json file.
3. Go to Settings > Metadata Actions -- [or just click here](http://localhost:8080/console/settings/metadata-actions) (you will need to enter your admin secret: `moocow`)
4. Select `Import Metadata` and select `default_metadata.json`

**Note:** By default, no tables are tracked. This is intentional as the Chinook database is being used for most of these and things can get a bit confusing if they are all tracked at once. I would recommend either using namespaces, or just track as needed.

## BigQuery

If you would like to have a working BigQuery database first obtain the BigQuery service account key JSON data (if you don't know, then ask). Then, enter the private key in your `default_metadata.json`.

You may also need to flip the [feature flags here](http://localhost:4200/settings/feature-flags) to see everything in your local instance correctly.

## MSSQL / SQL Server

In order to initialize and populate a MSSQL demo database automatically, we making use of the `mssql-tools` image. This image runs a few commands against the `mssql` image to create the demo data.

This scripting is done in `docker/DataSources/mssql/run-initialization.sh`.

After `run-initialization.sh` attempts to execute `docker/DataSources/mssql/mssql-setup.sql`, it creates the file `docker/DataSources/mssql/REMOVE_ME_TO_RERUN_SETUP.txt`. The presence of this `.txt` file will prevent the `mssql-setup.sql` script from executing again if the container is started again.

To force it to re-run, just delete `/DataSources/mssql/REMOVE_ME_TO_RERUN_SETUP.txt` and restart your container. Because `/DataSources/mssql` is mounted as a volume, it uses the local filesystem.

## Using Different HGE Images

You may need to replace the `image:` in the compose file with a different image. You can do this by downloading an image, and running `docker load -i <the new image>`. Once it's loaded copy the image name into the compose. Here's an example using a CI built image:

```yaml
image: hasura/graphql-engine:dev-e3bc433-main.ubuntu.arm64
```
