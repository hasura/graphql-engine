# Boilerplates for Google Cloud Functions and Hasura GraphQL Engine's Event Triggers
Sample cloud functions that can be triggered on changes in the database using GraphQL Engine's Event Triggers

## Pre-requisites

1. Google cloud account with billing enabled
2. `gcloud` CLI
3. Hasura GraphQL Engine

## 1. Create a Google Cloud Project

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

Create a service account and download the json file by following [this
guide](https://cloud.google.com/sql/docs/postgres/connect-kubernetes-engine#2_create_a_service_account).

Create a k8s secret with this service account:
```bash
kubectl create secret generic cloudsql-instance-credentials \
    --from-file=credentials.json=[PROXY_KEY_FILE_PATH]
```

Replace `[PROXY_KEY_FILE_PATH]` with the filename of the download json.

Create another secret with the database user and password
(Use the `[PASSWORD]` noted earlier):
```bash
kubectl create secret generic cloudsql-db-credentials \
    --from-literal=username=postgres --from-literal=password=[PASSWORD]
```

Get the `INSTANCE_CONNECTION_NAME`:
```bash
gcloud sql instances describe hge-pg
# it'll be something like this:
# connectionName: myproject1:us-central1:myinstance1
```

Edit `deployment.yaml` and replace `INSTANCE_CONNECTION_NAME` with the value.

Create the deployment:

```bash
kubectl create -f deployment.yaml
```

Ensure the pod is running:

```bash
kubectl get pods
```

Check logs if there are errors.

Expose GraphQL Engine on a Google LoadBalancer:

```bash
kubectl expose deploy/hasura-graphql-engine \
        --port 80 --target-port 8080 \
        --type LoadBalancer
```

Get the external IP:
```bash
kubectl get service
```

Open the console by navigating the the external IP in a browser.

Note down this IP as `HGE_IP`.

the READMEs in the folder for each use-case for setting up the following:

#. Schema design:  sample tables, relationships, etc. to be created using the Hasura GraphQL Engine's console.
#. Cloud Function: use the sample, working boilerplate example to setup a cloud function.
#. Trigger configuration: configure triggers for changes that will invoke a cloud function using the Hasura GraphQL Engine's console.

Once the trigger is created, goto `Data -> profile -> Insert row` and add new data in your table(s) to test the configured event trigger out. Check out individual READMEs for more details.
