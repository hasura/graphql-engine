# Azure Cloud Function Node.js with Hasura GraphQL Mutation

## Pre-requisites
1. You already have a Azure account with billing enabled.
2. Install [azure-cli](https://github.com/Azure/azure-cli)
3. Install [azure-functions-core-tools](https://github.com/Azure/azure-functions-core-tools)

## Setup Cloud Function
```bash
az group create --name 'my-functions-group' --location southindia

az storage account create --name 'myfunctionsstorage' --location southindia --resource-group 'my-functions-group' --sku Standard_LRS

az functionapp create --name 'myfunctionsapp' --storage-account 'myfunctionsstorage' --resource-group 'my-functions-group' --consumption-plan-location southindia

func azure login
func azure subscriptions set 'Free Trial'
func azure functionapp publish 'myfunctionsapp'
```

## Running locally
func host start

## Check Logs
func azure functionapp logstream 'myfunctionsapp'

## Notes
- Add a X-Function-Key header if Authorization level is enabled
- Set Environment variables `X_HASURA_ACCESS_KEY` and `X_HASURA_GRAPHQL_ENGINE`

