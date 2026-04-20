# DX: simplify subgraphs for getting started and federation workflows

Status: Review required

## subgraph init command
What this is: as a part of making it easier for people to work through the getting started guide, or to scaffold project files, allow the user to go from scratch to an API with only CLI commands.

init-ing a supergraph or connector will create files for both local and cloud environments. This is about what happens for the subgraphs.

```bash
# creates a subgraph folder and subgraph.yaml file
ddn subgraph init app

# add subgraph.yaml to the supergraph.yaml file and create an env file for the env
ddn supergraph add subgraph --supergraph supergraph.local.yaml --subgraph app/subgraph.yaml
touch app/.env.app.local

# create the supergraph build
ddn supergraph build --supergraph supergraph.yaml --env-file app/.env.app.local

# deploy to cloud
touch app/.env.app.cloud
ddn connector build create --connector connector.yaml --target-env-file app/.env.app.cloud
```

Order of env
- specify either as --env flags or --env-file flags, which can be specified multiple times
- all flags override the env specified in the subgraph.yaml file
- later flags override earlier flags

## --subgraph flag should take the file name

What this is about: on introducing federation, we realize that the primitive is the subgraph not the supergraph. The following commands are updated to consider the subgraph.yaml file instead of specifying a supergraph.yaml file and the subgraph by name.

```bash
ddn connector init mydb --subgraph app/subgraph.yaml --hub-connector hasura/postgres

ddn connector build create --connector mydb/connector.yaml --target-subgraph app/subgraph.yaml --target-connector-link mypg # use env file from subgraph.yaml

ddn connector build create --connector mydb/connector.yaml --target-subgraph app/subgraph.yaml --target-connector-link mypg --target-env-file app/.env.app.cloud

ddn model add --subgraph app/subgraph.yaml --connector-link mydb --name Album

ddn connector build get --connector-name mypg --subgraph-name app --project proj-1234

ddn connector build get --connector mypg/connector.yaml
```