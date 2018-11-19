# Hasura GraphQL Engine on Azure

Click the button below to create a Hasura GraphQL Engine container on
[Azure Container
Instances](https://azure.microsoft.com/en-us/services/container-instances/)
backed by an [Azure Database for
PostgreSQL](https://azure.microsoft.com/en-us/services/postgresql/) Server.
For a more detailed step-by-step guide on deplopying individual
resources to Azure using the CLI, refer to the 
[documentation](https://docs.hasura.io/1.0/graphql/manual/guides/deployment/azure-container-instances-postgres.html).

[![Deploy to Azure Button](https://azuredeploy.net/deploybutton.png)](https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fmaster%2finstall-manifests%2fazure-resource-manager%2fazuredeploy.json)

(The button opens Azure Portal, you might want to do a <kbd>Ctrl+Click</kbd>, to get it on a new tab)

## Pre-requisites

- A valid Azure Subscription ([click
  here](https://azure.microsoft.com/en-us/free/) for a free trial).
  
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
- **Postgres Version**: choose a version.
- **Database SKU Tier**: choose the SKU tier for the PostgreSQL service.
- **Database SKU Capacity**: choose the number of cores for the database.
- **Database SKU Size in MB**: choose the storage size for database (in MB).
- **Administrator Login Password**: enter a password for the database - minimum 8
  characters, must include lowercase, uppercase and numbers.
- **URL Encoded Admin Password**: if the admin password contains special
  characters (like `#`, `%`, `$` etc.), URL encode them (like `%40` for `@`) and
  enter here. If there are no special characters, just re-type the password.

![Azure Portal screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_template.png)

Once all entries are filled, agree to the terms and click the `Purchase` button.

The deployment will start now.

Click on the Notification Bell icon on the header bar and then click on
Deployment in Progress link.

On this screen, you can see progress for various steps in the deployment.

![Azure Portal deployment screen
screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_deployment_screen.png)

Once all steps are completed, click on the `Outputs` link on the sidebar.

![Azure Portal deployment output
screenshot](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_deployment_output.png)

The FQDN and IP address are shown in this screen. Copy the FQDN and paste it into
a browser. It will open up the Hasura GraphQL Engine console.

```
http://hasura-graphql-engine.centralindia.azurecontainer.io
```

![Console](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_console_graphiql.png)

## Next steps

- [Building your schema](https://docs.hasura.io/1.0/graphql/manual/schema/index.html)
- [GraphQL Queries](https://docs.hasura.io/1.0/graphql/manual/queries/index.html)
- [GraphQL Mutations](https://docs.hasura.io/1.0/graphql/manual/mutations/index.html)
- [GraphQL Subscriptions](https://docs.hasura.io/1.0/graphql/manual/subscriptions/index.html)
- [Event Triggers](https://docs.hasura.io/1.0/graphql/manual/event-triggers/index.html)
- [Authentication/Access control](https://docs.hasura.io/1.0/graphql/manual/auth/index.html)
- [Database Migrations](https://docs.hasura.io/1.0/graphql/manual/migrations/index.html)
- [Guides/Tutorials/Resources](https://docs.hasura.io/1.0/graphql/manual/guides/index.html)
