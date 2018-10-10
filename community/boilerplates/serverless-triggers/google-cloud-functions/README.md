# Boilerplates for Google Cloud Functions and Hasura GraphQL Engine's Event Triggers
Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers

**NOTE**
Some of the language/platforms are work in progress. We welcome contributions for the WIP langauages. See issues and the following checklist:

| Folder name | Use-case| Node.js(8) | Node.js(6) | Python
|-------------|---------|:--------:|:------:|:----:
| echo | echo the trigger payload  | ✅ | ✅ | ✅
| mutation | insert related data on an insert event using graphql mutation | ✅ | ✅ | ❌
| push-notification | send push notification on database event | ❌ | ❌ | ❌ 
| etl | transform the trigger payload and update an algolia index | ❌ | ❌ | ❌ 


## Pre-requisites

1. Google cloud account with billing enabled
2. `gcloud` CLI
3. Hasura GraphQL Engine

Get the `gcloud beta` component:

```bash
gcloud components update && gcloud components install beta
```
