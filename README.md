# Boilerplates for serverless cloud functions

This repository contains boilerplate functions for a set of sample use-cases for the following serverless or cloud function platforms:

* AWS Lambda

* Google Cloud Functions

* Microsoft Azure Functions

The following sample use-cases document a variety of asynchronous business logic operations that can be triggered by the Hasura GraphQL Engine when there's a database insert, update or delete:

* A basic "hello world" example to echo the webhook payload.

* Make a GraphQL mutation (*write related data back into the database*).

* Asynchronously send a FCM/APNS push notification.

* ETL: transform webhook payload and update an algolia index.

Some of the examples have a corresponding `great-first-issue` issue in the repository. Please checkout the checklist in the README in the cloud provider folders for such issues.
