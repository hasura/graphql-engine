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

4. Create an azure app service plan by using `az appservice plan create` command.
```
az appservice plan create --name myAppServicePlan --resource-group myResourceGroup --sku S1 --is-linux
```

When the App Service plan has been created, the Azure CLI shows information similar to the following example:

```
{ 
  "adminSiteName": null,
  "appServicePlanName": "myAppServicePlan",
  "geoRegion": "West Europe",
  "hostingEnvironmentProfile": null,
  "id": "/subscriptions/0000-0000/resourceGroups/myResourceGroup/providers/Microsoft.Web/serverfarms/myAppServicePlan",
  "kind": "linux",
  "location": "West Europe",
  "maximumNumberOfWorkers": 1,
  "name": "myAppServicePlan",
  < JSON data removed for brevity. >
  "targetWorkerSizeId": 0,
  "type": "Microsoft.Web/serverfarms",
  "workerTierName": null
}
```

5. Create a web app in the myAppServicePlan App Service plan with the `az webapp create` command. Don't forget to replace `<app name>` with a globally unique app name.
```
az webapp create --resource-group myResourceGroup --plan myAppServicePlan --name <app name> --deployment-container-image-name hasura/graphql-engine
```
In the preceding command, `--deployment-container-image-name` points to the public Docker Hub image `fossasia/susi_server`.

6. When the web app has been created, the Azure CLI shows output similar to the following example:

```
{
  "availabilityState": "Normal",
  "clientAffinityEnabled": true,
  "clientCertEnabled": false,
  "cloningInfo": null,
  "containerSize": 0,
  "dailyMemoryTimeQuota": 0,
  "defaultHostName": "<app name>.azurewebsites.net",
  "deploymentLocalGitUrl": "https://<username>@<app name>.scm.azurewebsites.net/<app name>.git",
  "enabled": true,
  < JSON data removed for brevity. >
}
```
7. Browse the app with `http://<app_name>.azurewebsites.net`
