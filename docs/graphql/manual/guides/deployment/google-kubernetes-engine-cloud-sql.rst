.. meta::
   :description: Deploy Hasura GraphQL engine on Google Cloud Platform with Kubernetes engine and Cloud SQL
   :keywords: hasura, docs, guide, deployment, google cloud, kubernetes, cloud sql

.. _deploy_gc_kubernetes:

Hasura GraphQL engine on Google Cloud Platform with Kubernetes engine and Cloud SQL
===================================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This is a guide on deploying the Hasura GraphQL engine on the `Google Cloud Platform
<https://cloud.google.com/>`__ using `Kubernetes engine
<https://cloud.google.com/kubernetes-engine/>`__ to run Hasura and PosgreSQL
backed by `Cloud SQL <https://cloud.google.com/sql/>`__. 

Prerequisites
-------------

- Google Cloud account with billing enabled (or a `free trial
  <https://cloud.google.com/free/>`__)
- ``gcloud`` CLI (`install <https://cloud.google.com/sdk/>`__)
- ``kubectl`` CLI (`install <https://kubernetes.io/docs/tasks/tools/install-kubectl/>`__)

The actions mentioned below can be done using the Google Cloud Console and the
``gcloud`` CLI. But, for the sake of simplicity in documentation, we are going
to use ``gcloud`` CLI, so that commands can be easily copy-pasted and executed. 

Once the CLI is installed, initialize the SDK:

.. code-block:: bash

   gcloud init

Create a Google Cloud Project
-----------------------------

A Google Cloud Project is used to group together resources. We'll create a
project called ``hasura`` for this guide.

.. code-block:: bash

   gcloud projects create hasura

Create a Google Cloud SQL Postgres instance
-------------------------------------------

Create a Cloud SQL Postgres instance called ``hasura-postgres`` in the
``us-west2`` region.

.. code-block:: bash

   gcloud sql instances create hasura-postgres --database-version POSTGRES_9_6 \
          --cpu 1 --memory 3840MiB --region us-west2 --project hasura

Once the instance is created, set a password for the default ``postgres`` user.
Make sure you substitute ``[PASSWORD]`` with a strong password.

.. code-block:: bash

   gcloud sql users set-password postgres --instance hasura-postgres \
          --password [PASSWORD] --project hasura

Create a Kubernetes Cluster
---------------------------

Before creating the cluster, we need to enable the Kubernetes engine API. Visit
the below link in a browser to enable the API. Replace ``hasura`` at the end
of the URL with your project name, in case you're not using the same name. Note
that, you will need a billing account added to the project to enable the API.

.. code-block:: bash

   https://console.cloud.google.com/apis/api/container.googleapis.com/overview?project=hasura

Once the API is enabled, create a new Kubernetes cluster called ``hasura-k8s``
in the ``us-west2-a`` zone with 1 node.

.. code-block:: bash

   gcloud container clusters create hasura-k8s --zone us-west2-a \
          --num-nodes 1 --project hasura

Set up Cloud SQL Proxy Credentials
----------------------------------

In order to connect to the Cloud SQL instance, we need to set up a proxy that will
forward connections from Hasura to the database instance. For that purpose, the
credentials to access the instance need to be added to the cluster.

Create a service account and download the JSON key file by following `this guide
<https://cloud.google.com/sql/docs/postgres/sql-proxy#create-service-account>`__.

Or if you're overwhelmed with that guide, ensure the following:

1. Enable `Cloud SQL Admin API
   <https://console.developers.google.com/apis/api/sqladmin.googleapis.com/overview?project=hasura>`__
   for your project.
2. Create a new `Service Account
   <https://console.cloud.google.com/iam-admin/serviceaccounts/?project=hasura>`__.
3. Select ``Cloud SQL Admin`` as the role.
4. Click ``Create Key`` to download the JSON file.

Create a Kubernetes secret with this JSON key file; replace
``[JSON_KEY_FILE_PATH]`` with the filename including the complete path of the
download JSON key file.

.. code-block:: bash

   kubectl create secret generic cloudsql-instance-credentials \
           --from-file=credentials.json=[JSON_KEY_FILE_PATH]

Create another secret with the database username and password (use the
``[PASSWORD]`` used earlier):

.. code-block:: bash

   kubectl create secret generic cloudsql-db-credentials \
           --from-literal=username=postgres --from-literal=password=[PASSWORD]

Deploy the Hasura GraphQL engine
--------------------------------

Download the ``deployment.yaml`` file:

.. code-block:: bash

   wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/google-cloud-k8s-sql/deployment.yaml

Get the ``[INSTANCE_CONNECTION_NAME]`` using the following command and note it
down.

.. code-block:: bash

   gcloud sql instances describe hasura-postgres \
          --format="value(connectionName)" --project hasura

Edit ``deployment.yaml`` and replace ``[INSTANCE_CONNECTION_NAME]`` with this
value. It should look like ``hasura:us-west2:hasura-postgres`` if you've
followed this guide without modifying any names.

Create the deployment:

.. code-block:: bash

   kubectl apply -f deployment.yaml

Ensure the pods are running:

.. code-block:: bash

   kubectl get pods

If there are any errors, check the logs of the GraphQL engine:

.. code-block:: bash

   kubectl logs deployment/hasura -c graphql-engine

Expose GraphQL engine (HTTP)
---------------------

Now that we have Hasura running, let's expose it on an IP using a LoadBalancer.

.. code-block:: bash

   kubectl expose deploy/hasura \
        --port 80 --target-port 8080 \
        --type LoadBalancer

Wait for the external IP to be allocated, check the status using the
command below. It usually takes a couple of minutes.

.. code-block:: bash

   kubectl get service

Once the IP is allocated, visit the IP in a browser and it should open the
console.

Expose GraphQL engine (HTTPS)
---------------------

Let's expose Hasura with `Ingres 
<https://cloud.google.com/kubernetes-engine/docs/concepts/ingress/>`_. Create service:

.. code-block:: yaml


	apiVersion: v1
	kind: Service
	metadata:
	  labels:
	    app: hasura
	  name: hasura
	  namespace: default
	spec:
	  ports:
	    - protocol: TCP
	      port: 80
	      targetPort: 8080
	  selector:
	    app: hasura
	  type: NodePort

Create Managed Certificate:

.. code-block:: yaml

	apiVersion: networking.gke.io/v1beta1
	kind: ManagedCertificate
	metadata:
	  name: hasura-cert
	spec:
	  domains:
	    - example.com


Create Ingress:

.. code-block:: yaml

	apiVersion: extensions/v1beta1
	kind: Ingress
	metadata:
	  name: basic-ingress
	  annotations:
	    networking.gke.io/managed-certificates: "hasura-cert"
	spec:
	  rules:
	    - host: example.com
	      http:
	        paths:
	          - backend:
	              serviceName: hasura
	              servicePort: 80





Troubleshooting
---------------

Check the status for pods to see if they are running. If there are any errors,
check the logs of the GraphQL engine:

.. code-block:: bash

   kubectl logs deployment/hasura -c graphql-engine

You might also want to check the logs for cloudsql-proxy:

.. code-block:: bash

   kubectl logs deployment/hasura -c cloudsql-proxy

The username and password used by Hasura to connect to the database comes from a
Kubernetes secret object ``cloudsql-db-credentials`` that we created earlier.

Tearing down
------------

To clean up the resources created, just delete the Google Cloud Project:

.. code-block:: bash

   gcloud projects delete hasura
