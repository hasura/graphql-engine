.. .. meta::
   :description: Part 4 of a set of learning exercises meant for exploring Hasura in detail. This part shows you how to consume the data microservice's instant JSON API.
   :keywords: hasura, getting started, step 4, data API

===================================
Part IV: Explore the Hasura cluster
===================================

A Hasura cluster is essentially a kubernetes cluster that has Hasura APIs,
a controller to manage configuration changes across the cluster, and an SSH/git-push microservice
to help you `git push` to deploy easily.


Get cluster information
-----------------------

Inside your project directory, run:

.. code-block:: bash

   $ hasura cluster status

::

    Cluster name:       test42
    Cluster alias:      hasura
    Kubectl context:    test42
    Platform version:   v0.15.31
    Cluster state:      Synced
    
    • Cluster configuration:
    no changes
    
    • To view the status of microservices use:
      $ hasura ms list

The first section shows general information about the cluster.

The cluster configuration section will tell if there is a difference between the local and remote cluster configurations.

Microservices
-------------

Microservices are basically the individual ‘apps’ that run on the cluster.

Hasura microservices are the default Hasura specific microservices that run on your cluster. These include the backend APIs provided by Hasura and other components of the Hasura platform.

You can also add your own custom microservices to your project.

To list the microservices running in your Hasura cluster, run the following command from the project directory.

.. code-block:: bash

    $ hasura microservice list

::

    USER MS NAME  STATUS  REPLICAS  INTERNAL-URL  EXTERNAL-URL

    HASURA MS NAME  STATUS   REPLICAS  INTERNAL-URL                      EXTERNAL-URL
    auth            Running  1/1       auth.test42-hasura:80             http://auth.test42.hasura-app.io/
    data            Running  1/1       data.test42-hasura:80             http://data.test42.hasura-app.io/
    filestore       Running  1/1       filestore.test42-hasura:80        http://filestore.test42.hasura-app.io/
    gateway         Running  1/1
    le-agent        Running  1/1
    notify          Running  1/1       notify.test42-hasura:80           http://notify.test42.hasura-app.io/
    platform-sync   Running  1/1
    postgres        Running  1/1       postgres.test42-hasura:5432
    session-redis   Running  1/1       session-redis.test42-hasura:6379
    sshd            Running  1/1



Currently, there are no custom user microservices running on the cluster. The Hasura microservices running on the cluster are as follows:

auth
^^^^
The auth microservice handles user authentication for your Hasura cluster.

data
^^^^
The data microservice on Hasura exposes an HTTP/JSON API over a PostgreSQL database.

filestore
^^^^^^^^^
The filestore microservice lets users upload and store files on the Hasura project and also download when required.

gateway
^^^^^^^
This microservice exposes your project to the outside world. It authenticates and redirects external requests to the appropriate microservice inside the cluster.

le-agent
^^^^^^^^
This is the LetsEncrypt agent which is responsible for generating SSL certificates.

notify
^^^^^^
This microservice handles the Email/SMS communication.

platform-sync
^^^^^^^^^^^^^
This microservice keeps the cluster in sync with the configuration.

postgres
^^^^^^^^
This microservice hosts the PostgreSQL database.

session-redis
^^^^^^^^^^^^^
This is a Redis instance used by the auth microservice.

sshd
^^^^
This microservice handles SSH access to the cluster.


Next: Start building a blog-engine app!
---------------------------------------

Next, head to :doc:`build-blog-app`
