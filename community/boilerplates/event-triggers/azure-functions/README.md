# Boilerplates for Azure Cloud Function serverless functions and Hasura GraphQL Engine's Event Triggers

**NOTE**
Some of the language/platforms are work in progress. We welcome contributions for the WIP languages. See issues and the following checklist:

| Folder name | Use-case| Javascript | Java | C# | F#
|-------------|---------|:--------:|:------:|:----:|:---:
| echo | echo the trigger payload  | ✅ | ❌ | ❌ | ❌ 
| mutation | insert related data on an insert event using graphql mutation | ✅ | ❌ | ❌ | ❌ 
| push-notification | send push notification on database event | ❌ | ❌ | ❌ | ❌ 
| etl | transform the trigger payload and update an algolia index | ❌ | ❌ | ❌ | ❌

## Pre-requisites
1. Running instance of Hasura GraphQL
2. You already have a Azure account with billing enabled.
3. Install [azure-cli](https://github.com/Azure/azure-cli)
4. Install [azure-functions-core-tools](https://github.com/Azure/azure-functions-core-tools)
