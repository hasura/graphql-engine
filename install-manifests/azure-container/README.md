# Hasura GraphQL Engine on Azure

_This manifest is about using Hasura with an existing database. If you're looking for a manifest that provisions a new Postgres server also, checkout [`../azure-container-with-pg`](../azure-container-with-pg) directory._

Click the button below to create a Hasura GraphQL Engine container on
[Azure Container
Instances](https://azure.microsoft.com/en-us/services/container-instances/)
backed by an existing Postgres server (e.g. [Azure Database for
PostgreSQL](https://azure.microsoft.com/en-us/services/postgresql/)).
For a more detailed step-by-step guide on deplopying individual
resources to Azure using the CLI, refer to the 
[documentation](https://hasura.io/docs/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html).

[![Deploy to Azure Button](https://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-container%2fazuredeploy.json)

(The button opens Azure Portal, you might want to do a <kbd>Ctrl+Click</kbd>, to get it on a new tab)

## Pre-requisites

- A valid Azure Subscription ([click
  here](https://azure.microsoft.com/en-us/free/) for a free trial).
- An existing PostgreSQL database.
  
## Instructions

Once you click the button, it will take you to the Azure Portal, where you might be
prompted to login first.

A custom deployment screen will show up - enter the following information, as shown in
the screenshot that follows:

- **Subscription**: choose an Azure subscription.
- **Resource Group**: choose an existing one or create a new one.
- **Location**: choose a location for the resource group (note: Azure Container
  Instances and Database for PostgreSQL may not be available in all locations.
  [Click
  here](https://azure.microsoft.com/en-us/global-infrastructure/services/?products=postgresql,container-instances&regions=all)
  to check availability.)
- **Name**: enter a unique name for the deployment, this name is used for
  provisioning a DNS label for the container, so it needs to be globally unique.
- **Postgres Host (Server Name)**: enter the Postgres server name, obtained from Azure Portal or otherwise. Typically, it would look like pg-server.postgres.database.azure.com.
- **Postgres Port**: choose the PostgreSQL port, default is 5432.
- **Postgres Username (Server Admin Login Name)**: enter the login name for Postgres server, typically of the form hasura@pg-server for Azure.
- **Postgres Password**: enter the password for Postgres user.
- **Postgres Database Name**: enter the name of database that Hasura should connect to.


_(Note: Make sure you’ve allowed access for Azure services to the Postgres Server. This setting can be found under the connection security tab on Azure Portal.)_

![Azure Portal screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_standalone_template.png)

Once all entries are filled, agree to the terms and click the `Purchase` button.

The deployment will start now.

Click on the Notification Bell icon on the header bar and then click on
Deployment in Progress link.

On this screen, you can see progress for various steps in the deployment.

![Azure Portal deployment screen
screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_standalone_deploy_complete.png)

Once all steps are completed, click on the `Outputs` link on the sidebar.

![Azure Portal deployment output
screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_deployment_output.png)

The FQDN and IP address are shown in this screen. Copy the FQDN and paste it into
a browser. It will open up the Hasura GraphQL Engine console.

```
http://hasura-graphql-engine.centralindia.azurecontainer.io
```

![Console](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_console_graphiql.png)

## Track Tables

Once you open the console, switch to Data tab and you’ll be able to see the tables in the database. You might have to switch the schema (dropdown on the top of sidebar) if your tables are not in the default public schema.

![Console Track Table](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/console_track_tables.png)


## Next steps

- [Building your schema](https://hasura.io/docs/1.0/graphql/manual/schema/index.html)
- [GraphQL Queries](https://hasura.io/docs/1.0/graphql/manual/queries/index.html)
- [GraphQL Mutations](https://hasura.io/docs/1.0/graphql/manual/mutations/index.html)
- [GraphQL Subscriptions](https://hasura.io/docs/1.0/graphql/manual/subscriptions/index.html)
- [Event Triggers](https://hasura.io/docs/1.0/graphql/manual/event-triggers/index.html)
- [Authentication/Access control](https://hasura.io/docs/1.0/graphql/manual/auth/index.html)
- [Database Migrations](https://hasura.io/docs/1.0/graphql/manual/migrations/index.html)
- [Guides/Tutorials/Resources](https://hasura.io/docs/1.0/graphql/manual/guides/index.html)
