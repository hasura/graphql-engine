# Boilerplates for Azure Cloud Functions and Hasura GraphQL Engine's Event Triggers
Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers

## Pre-requisites

1. Azure  account with billing enabled
2. `azure` CLI
3. Hasura GraphQL Engine

## 1. Create an Azure Project

## 2. Create a cloud SQL Postgres instance

?

## 3. Create a Kubernetes cluster

Needed?

## 4. Configure and deploy GraphQL Engine

AKS?

Note down this IP as `HGE_IP`.

the READMEs in the folder for each use-case for setting up the following:

#. Schema design:  sample tables, relationships, etc. to be created using the Hasura GraphQL Engine's console.
#. Cloud function: use the sample, working boilerplate example to setup a Lambda function.
#. Trigger configuration: configure triggers for changes that will invoke a cloud function using the Hasura GraphQL Engine's console.

Once the trigger is created, goto `Data -> profile -> Insert row` and add new data in your table(s) to test the configured event trigger out. Check out individual READMEs for more details.
