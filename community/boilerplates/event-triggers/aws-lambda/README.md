# Boilerplates for AWS Lambda serverless functions and Hasura GraphQL Engine's Event Triggers

Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers.

These are organized in language-specific folders.

**NOTE**
Some of the language/platforms are work in progress. We welcome contributions for the WIP langauages. See issues and the following checklist:

| Folder name | Use-case| Node.js(6) | Python | Java | Go | C# | Ruby
|-------------|---------|:--------:|:------:|:----:|:---:|:---:|:---:
| echo | echo the trigger payload  | ✅ | ✅ | ❌ | ✅ | ❌ | ✅
| mutation | insert related data on an insert event using graphql mutation | ✅ | ✅ | ❌ | ✅ | ❌ | ✅
| push-notification | send push notification on database event | ❌ | ❌ | ❌ | ❌ | ❌ | ❌
| etl | transform the trigger payload and update an algolia index | ❌ | ❌ | ❌ | ❌ | ❌ | ❌



## Pre-requisites

1. AWS account with billing enabled
2. Hasura GraphQL Engine
