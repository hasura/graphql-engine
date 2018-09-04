# Boilerplates for AWS Lambda serverless functions and Hasura GraphQL Engine's Event Triggers

Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers.

These are organized in language-specific folders.

**Checklist**

| Folder name | Use-case       | Node.js | Python | Java | Go | C#
|-------------|---------|--------|------|----|---|---
| simple-echo | echo the trigger payload  | - [x] | - [x]  | - [ ]  | - [ ]  | - [ ]
| mutation-trigger | insert related data on an insert event using graphql mutation | - [x] | - [x]  | - [ ]  | - [ ]  | - [ ]
| fcm-apns-push-notification | send push notification on database event | - [ ] | - [ ]  | - [ ]  | - [ ]  | - [ ]
| etl-example | transform the trigger payload and update an algolia index | - [ ] | - [ ]  | - [ ]  | - [ ]  | - [ ]

## Pre-requisites

1. AWS account with billing enabled
2. Hasura GraphQL Engine

### AWS setup
You need to create a corresponding AWS Lambda for each of these examples.

As the Hasura event system takes webhooks as the event triggers, we need to expose these Lambdas as webhooks. To do that, we need to add the API gateway trigger to each lambda and add an API to it.
