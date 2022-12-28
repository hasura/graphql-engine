# Hasura GraphQL Engine on Azure

This manifest is about using Hasura on Azure App Service. It includes an option to work with an existing database or use an internally hosted one. 

## Pre-requisites

- A valid Azure Subscription ([click
  here](https://azure.microsoft.com/en-us/free/) for a free trial).
- Installed [Azure CLI](https://learn.microsoft.com/en-us/cli/azure/get-started-with-azure-cli).
  
## Notes
- Before you start, edit the docker-compose.yml file to a new admin password. Or you can comment it out if you do not want a password protecting your deployment.
- The deployment will run a postgres server for metadata inside of your AppService. This is good for a single deployment. Yet this does not scale for production! If you are looking for production, edit the docker-compose-externalpostgres.yml file to include your existing postgres database.

## Deployment
Open up your terminal with Azure CLI installed
- az group create --name hasura-test-appservice --location westeurope
- az appservice plan create --name AppServicePlan4Hasura --resource-group hasura-test-appservice --is-linux

If you want to leverage the quick and dirty "All-In-One" deployment
- az webapp create --resource-group hasura-test-appservice --plan AppServicePlan4Hasura --name <uniqueappname> --multicontainer-config-type compose --multicontainer-config-file docker-compose.yml

Or if you want the one with the external database
- az webapp create --resource-group hasura-test-appservice --plan AppServicePlan4Hasura --name <uniqueappname> --multicontainer-config-type compose --multicontainer-config-file docker-compose-externalpostgres.yml

That's all!

## Access Deployment

The FQDN and IP address are shown in this screen. Copy the FQDN and paste it into
a browser. It will open up the Hasura GraphQL Engine console.

```
http://<uniqueappname>.azurewebsites.net
```

## Troubleshoot

What if something goes wrong? In the Azure Portal, go the the "App Service" resource called "<uniquename>" and there to "Deployment Center". Here you can see a tab "Logs", which will show you the raw docker logs as they pass by. Be aware that the first deployment can take several minutes to get everything unpacked.

If you forgot to change your admin key, then this is set to "myadminsecretkey". ;-)

## Track Tables

Once you open the console, switch to Data tab and you’ll be able to see the tables in the database. You might have to switch the schema (dropdown on the top of sidebar) if your tables are not in the default public schema.

![Console Track Table](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/console_track_tables.png)


## Next steps

- [Building your schema](https://hasura.io/docs/latest/graphql/core/schema/index.html)
- [GraphQL Queries](https://hasura.io/docs/latest/graphql/core/queries/index.html)
- [GraphQL Mutations](https://hasura.io/docs/latest/graphql/core/mutations/index.html)
- [GraphQL Subscriptions](https://hasura.io/docs/latest/graphql/core/subscriptions/index.html)
- [Event Triggers](https://hasura.io/docs/latest/graphql/core/event-triggers/index.html)
- [Authentication/Access control](https://hasura.io/docs/latest/graphql/core/auth/index.html)
- [Database Migrations](https://hasura.io/docs/latest/graphql/core/migrations/index.html)
- [Guides/Tutorials/Resources](https://hasura.io/docs/latest/graphql/core/guides/index.html)
