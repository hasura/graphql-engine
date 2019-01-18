# Setup tables
1. Create table:

```
notes:
  id: int
  note: text
```

# Setup Cloud Function
1. Run the following commands to deploy:
```bash
az group create --name 'my-functions-group' --location southindia

az storage account create --name 'myfunctionsstorage' --location southindia --resource-group 'my-functions-group' --sku Standard_LRS

az functionapp create --name 'myfunctionsapp' --storage-account 'myfunctionsstorage' --resource-group 'my-functions-group' --consumption-plan-location southindia

func azure login
func azure subscriptions set 'Free Trial'
func azure functionapp publish 'myfunctionsapp'
```
2. Set Environment variables `ACCESS_KEY` and `HGE_ENDPOINT`
3. Add a X-Function-Key header if Authorization level is enabled

# Running locally
`func host start`

# Check Logs
`func azure functionapp logstream 'myfunctionsapp'`

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste your function URL as the webhook.
