# Boilerplates for serverless cloud functions

This repository contains boilerplate functions for a set of sample use-cases for different serverless or cloud function platforms. These functions implement sample use-cases of the different types of asynchronous business logic operations that can be triggered by the Hasura GraphQL Engine on database insert, update or delete. Examples in this repository support the following cloud function platforms:

   * AWS Lambda

   * Google Cloud Functions

   * Microsoft Azure Functions

   Note: *If you want to add support for other platforms, please submit a PR or create an issue and tag it with `help-wanted`*


## Events Trigger and Serverless functions architecture

![Architecture diagram](assets/basic-event-triggers-arch-diagram.png)

## Documented examples

* A simple "hello world" example to echo the database changes.

* Make a GraphQL mutation (*write related data back into the database*).

* Asynchronously send a FCM/APNS push notification.

* ETL or data transformation: transform the webhook payload and update an algolia index.

Note: Some of the examples have a corresponding `great-first-issue` issue in the repository. Please checkout the checklist in the README in the cloud provider folders for such issues.


## Repo Structure

Boilerplates have been organised into top-level folders for each cloud function platform. Inside each such folder, there's a folder for each use-case and language supported by that cloud platform. The README for each cloud function provider has a list of available boilerplates. The following is a representative tree of the folder structure:


    .
    ├── aws-lambda
    |   |── README.md
    |   |── nodejs-hello-world-echo-on-insert-update-delete-trigger
    |   |── python-hello-world-echo-on-insert-update-delete-trigger
    |
    ├── microsoft-azure-functions
    |   |── README.md 
    |   |── javascript-hello-world-echo-on-insert-update-delete-trigger
    |   |── java-hello-world-echo-on-insert-update-delete-trigger
    |
    
## Contributing and boilerplate requests
Want to contribute to this repo? Issues marked `good-first-issue` or `help-wanted` are a good place to begin. Please submit a P.R for new boilerplates (other use-cases or cloud function providers/platforms like Apache OpenWhisk, etc.). You can also create issues to request new boilerplates.


