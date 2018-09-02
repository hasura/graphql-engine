# Boilerplates for AWS Lambda serverless functions and Hasura GraphQL Engine's Event Triggers
Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers.

**Checklist**

| Use-case       | Node.js | Python | Java | Go | C# 
|----------------|---------|--------|------|----|---
| A simple "hello world" example to echo the database changes | - [x] | - [ ]  | - [ ]  | - [ ]  | - [ ] 

| Task           | Time required | Assigned to   | Current Status | Finished | 
|----------------|---------------|---------------|----------------|-----------|
| Calendar Cache | > 5 hours  | @georgehrke | in progress | - [x] ok?
| Object Cache   | > 5 hours  | @georgehrke | in progress | [x] item1<br/>[ ] item2
| Object Cache   | > 5 hours  | @georgehrke | in progress | <ul><li>- [x] item1</li><li>- [ ] item2</li></ul>
| Object Cache   | > 5 hours  | @georgehrke | in progress | <ul><li>[x] item1</li><li>[ ] item2</li></ul>



## Pre-requisites

1. AWS  account with billing enabled
2. `AWS` CLI
3. Hasura GraphQL Engine

## 1. Create a AWS Project

## 2. Create a Google Cloud SQL Postgres instance

Create a PostgreSQL instance:

```bash
gcloud sql instances create hge-pg --database-version=POSTGRES_9_6 \
       --cpu=1 --memory=3840MiB --region=asia-south1
```

Set a password for `postgres` user:

```bash
gcloud sql users set-password postgres no-host --instance=hge-pg \
       --password=[PASSWORD]
```

Make a note of the `[PASSWORD]`.

## 3. Create a Kubernetes cluster

```bash
gcloud container clusters create hge-k8s \
      --zone asia-south1-a \
      --num-nodes 1
```

## 4. Configure and deploy GraphQL Engine

ECS?

Note down this IP as `HGE_IP`.

the READMEs in the folder for each use-case for setting up the following:

#. Schema design:  sample tables, relationships, etc. to be created using the Hasura GraphQL Engine's console.
#. Lambda: use the sample, working boilerplate example to setup a Lambda function.
#. Trigger configuration: configure triggers for changes that will invoke a cloud function using the Hasura GraphQL Engine's console.

Once the trigger is created, goto `Data -> profile -> Insert row` and add new data in your table(s) to test the configured event trigger out. Check out individual READMEs for more details.
