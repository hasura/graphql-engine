## Todo GraphQL Migrations

Update config.yaml to point to the right graphql-engine endpoint with/without admin_secret.

Run the following commands:
```
$ hasura migrate apply
```
This will apply the migrations.

```
$ hasura metadata apply
```
This will apply the graphql-engine metadata.


