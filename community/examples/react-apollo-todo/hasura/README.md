## Todo GraphQL Migrations

Update config.yaml to point to the right graphql-engine endpoint with/without access_key.

Run the following commands:
```
$ hasura migrate apply
```
This will apply the migrations.

```
$ hasura metadata apply
```
This will apply the graphql-engine metadata.


