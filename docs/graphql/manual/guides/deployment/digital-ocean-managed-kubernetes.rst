Hasura GraphQL Engine on DigitalOcean Managed Kubernetes
===============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

DigitalOcean now offers a managed Kubernetes service, making it a perfect hosting provider to
quickly setup and deploy Hasura GraphQL Engine without managing servers.

Quickstart
----------

Following this tutorial means you will need the following installed on your machine:

- ``doctl`` (https://github.com/digitalocean/doctl/blob/master/README.md)
- ``kubectl`` (https://kubernetes.io/docs/tasks/tools/install-kubectl/)

Setting up a Managed Database
----------

Hasura needs a PostgreSQL database, to keep things easy, we can use a Digital Ocean Managed database.

Login to your Digital Ocean console and create a new managed database, select the latest version
of PostgreSQL and name your database ``hasura-demo``.

.. thumbnail:: ../../../../img/graphql/manual/guides/digital-ocean-managed-kubernetes/create-managed-database.png

While that is provisioning, we can move onto the next steps.

Setting up a Managed Cluster
----------

The fastest way to setup a Kubernetes cluster on Digital Ocean is to use the `doctl` command line tool.
Let's create a cluster and call it ``hasura-demo``.

.. note::

  Feel free to rename this to match your project name but the remainder of this tutorial will refer to
  the cluster as ``hasura-demo`` so don't forget to change the name to match!

The following command will create a cluster using the ``doctl`` command and spin up three worker nodes.

.. code-block:: bash

  doctl kubernetes cluster create hasura-demo

.. thumbnail:: ../../../../img/graphql/manual/guides/digital-ocean-managed-kubernetes/create-cluster.png

This is the longest step in this tutorial. The remaining steps will go rather quick.

Once the cluster is created, we need to configure your local ``kubectl`` commands to point to the newly created cluster.
The ``doctl`` commands makes this really simple:

.. code-block:: bash

  doctl kubernetes cluster kubeconfig save hasura-demo

.. thumbnail:: ../../../../img/graphql/manual/guides/digital-ocean-managed-kubernetes/configure-kubectl.png

To verify that we are connected to the new cluster, we can list the pods within the clister.
Since we have not deployed anything, there should be no running clusters.

.. code-block:: bash

  kubectl get pods

.. thumbnail:: ../../../../img/graphql/manual/guides/digital-ocean-managed-kubernetes/get-pods.png

.. note::

  Notice that we are no longer using the ``doctl`` command as our ``kubectl`` is
  now configured to talk directly to our cluster!

Congratulations! You have now deployed a Kubernetes cluster on Digital Ocean and
configured your local machine to talk to the cluster! Now its time to start deploying
your applications and, more importantly, the Hasura GraphQL Engine!

Creating and Managing Secrets
----------

With the cluster setup and our ``kubectl`` configured to communicate to the new cluster, we
need to get some prep-work before deploying Hasura to the cluster. The first thing to discuss
is managing and storing secrets.

Kuberenetes uses YAML files to store our applications
configuration and state, which means that in order to connect to the database we need to
store the database connection string configuration inside of the YAML.

Luckily, Kubernetes ships with a secrets manager! In order to connect to
the database, we need to grab the PostgreSQL connection string from the
Digital Ocean managed database.

TODO IMAGE

Most examples of ``manifest.yaml`` files for Kubernetes will store items like usernames and passwords in plaintext, this is a really bad practice!
Since Kuberenetes has a secrets manager, we can create a ``secrets.yaml`` file to store the credentials.

.. note::

  Kubernetes requires that credentials placed in the YAML file be base64 encoded. The keyword
  is ``encoded``, this does not mean encrypted and means that your ``secrets.yaml`` file **should not**
  be stored in your version control as it is essentially storing the content in plain text.

Using the database connection string from the last step, run the following commands:

.. code-block:: bash

  echo -n 'postgresql://username:password@host:port/somedb' | base64
  echo -n 'adminsecretpassword' | base64

.. note::

  Don't forget to replace with your database credentials and admin console password. Also, remove the
  ``?sslmode=require`` at the end of the connection string.

Create the ``secrets.yaml`` file and replace the content with the output of the previous command.

.. code-block:: yaml

  apiVersion: v1
  kind: Secret
  metadata:
    name: hasura-secrets
  data:
    db: CHANGEME
    admin: CHANGEME

This will create a new secret with the name ``hasura-secrets`` and place two keys within
secret with the keys ``db`` and ``admin``.

.. note::

  You can change these, but don't forget to update the keys or secret name in ``manifest.yaml``
  that is created in the next step.

The last step is to deploy the secret! Run the following command:

.. code-block:: bash

  kubectl apply -f secrets.yaml

Deploying Hasura GraphQL Engine
----------

Now onto the final step! So far we have created a managed database, a managed cluster, and properly
stored our secrets for the application to use. Now it is time to create the ``manifest.yaml`` and deploy our
application.

.. note::

  By default Kubernetes does not expose your application pods or services. In order to expose our application
  to the world, it will need an ingress controller. Digital Ocean provides this for use with the use of a
  Load Balancer. Our ``manifest.yaml`` contains a Custom Resource Definition to tell Digital Ocean to
  create the load balancer and forward it to our application.

Create the ``manifest.yaml`` file with the following contents:

.. code-block:: yaml

  ---
  kind: Service
  apiVersion: v1
  metadata:
    name: hasura
  spec:
    type: LoadBalancer
    selector:
      app: hasura
    ports:
      - name: http
        protocol: TCP
        port: 80
        targetPort: 8080
  ---
  apiVersion: apps/v1
  kind: Deployment
  metadata:
    labels:
      app: hasura
      hasuraService: custom
    name: hasura
    namespace: default
  spec:
    replicas: 3
    selector:
      matchLabels:
        app: hasura
    template:
      metadata:
        creationTimestamp: null
        labels:
          app: hasura
      spec:
        containers:
        - image: hasura/graphql-engine:v1.0.0-beta.8
          imagePullPolicy: IfNotPresent
          name: hasura
          env:
          - name: HASURA_GRAPHQL_DATABASE_URL
            valueFrom:
              secretKeyRef:
                name: hasura-secrets
                key: db
          - name: HASURA_GRAPHQL_ENABLE_CONSOLE
            value: "true"
          - name: HASURA_GRAPHQL_ADMIN_SECRET
            valueFrom:
              secretKeyRef:
                name: hasura-secrets
                key: admin
          ports:
          - containerPort: 8080
            protocol: TCP
          resources: {}

This will tell Kubernetes to create a Load Balancer, spin up 3 replicas (containers) of the Hasura
application, and tells the Hasura application to create the environment variables
``HASURA_GRAPHQL_DATABASE_URL`` and ``HASURA_GRAPHQL_ADMIN_SECRET`` but pull the values
from the secrets manager.

There are three environment variables currently defined for our service, two are stored in secrets
manager but the other is an option for enabling the console and does not contain sensitive information,
Therefore it is only defined as an envrionment variable and not a secret.

To deploy the application, run the following command:

.. code-block:: bash

  kubectl apply -f manifest.yaml

Login to your Digital Ocean control panel (https://cloud.digitalocean.com/networking/load_balancers)
and you should have a newly created load balancer, grab the IP and visit your new GraphQL API powered by Hasura!
