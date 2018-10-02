# Deploying guide on Microsoft Azure cloud
You can deploy Graphql engine on microsoft azure


1. Use free trial by signing up on [Azure free trial](https://azure.microsoft.com/en-in/free/). You will get $200 free azure credits.

2. From your dashboard launch Azure cloud shell.

3. Create a resource group. A resource group is a logical container into which Azure resources like web apps, databases, and storage accounts are deployed and managed. For example, you can choose to delete the entire resource group in one simple step later.
* In the Cloud Shell, create a resource group with the `az group create` command.

```
az group create --name myResourceGroup --location "West Europe"
```

* You generally create your resource group and the resources in a region near you.
*When the command finishes, a JSON output shows you the resource group properties.

4. Create an azure container instance by using `az container create` command.
```
az container create --resource-group myResourceGroup --name mycontainer --image hasura/graphql-engine --dns-name-label graphql-engine --ports 80
```

when the command finishes execution, it will rethrn the logs in JSON

5. Wait for few seconds, when the Container Instance has been created, you can check the status on ` az container show` command

```
az container show --resource-group myResourceGroup --name mycontainer --query "{FQDN:ipAddress.fqdn,ProvisioningState:provisioningState}" --out table
```

It will show the following output 
``` 
FQDN                               ProvisioningState
---------------------------------  -------------------
graphql-engine.eastus.azurecontainer.io  Succeeded
```

6. When the container is succeed navigate to its FQDN in your browser.
