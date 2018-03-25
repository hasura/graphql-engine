.. .. meta::
   :description: Part 4 of a set of learning exercises meant for exploring Hasura in detail. This part shows you how to consume the data microservice's instant JSON API.
   :keywords: hasura, getting started, step 4, data API

===================================
Part IV: Explore the Hasura cluster
===================================

A Hasura cluster is essentially a kubernetes cluster that has 4 Hasura APIs,
a controller to manage configuration changes across the cluster, and an SSH/git-push microservice
to help you `git push` to deploy easily.


Get cluster information and the microservices running
-----------------------------------------------------

Inside your project directory, run:

.. code-block:: console

   $ hasura cluster status

the output of which looks like:

::

  INFO Reading cluster status...                    
  INFO Status:                                      
  Cluster Name:       archness84
  Cluster Alias:      hasura
  Kube Context:       archness84
  Platform Version:   v0.15.3
  Cluster State:      Synced

  INFO Cluster configuration:                       
  no changes

  INFO Gateway warnings:                            
  none

  INFO Custom microservices:                             
  NAME      STATUS    ENDPOINT
  adminer   Running   https://adminer.archness84.hasura-app.io

  INFO Hasura microservices:                             
  NAME            STATUS    ENDPOINT
  auth            Running   https://auth.archness84.hasura-app.io
  data            Running   https://data.archness84.hasura-app.io
  filestore       Running   https://filestore.archness84.hasura-app.io
  gateway         Running   
  le-agent        Running   
  notify          Running   https://notify.archness84.hasura-app.io
  platform-sync   Running   
  postgres        Running   
  session-redis   Running   
  sshd            Running   


The first section shows general information about the cluster.

The cluster configuration section will tell if there is a difference between the local and remote cluster configurations.

The microservices section shows the custom and Hasura microservices running in your cluster along with their http endpoint.

Hasura Services
---------------

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

Next, head to :doc:`Part V: Building a blog-engine app <5-build-blog-app>`
