.. meta::
   :description: Part 4 of a set of learning exercises meant for exploring Hasura in detail. This part shows you how to consume the data service's instant JSON API.
   :keywords: hasura, getting started, step 4, data API

===================================
Part IV: Explore the Hasura cluster
===================================

A Hasura cluster is essentially a kubernetes cluster that has 4 Hasura APIs,
a controller to manage configuration changes across the cluster, and a SSH/git-push service
to help you `git push` to deploy easily.


Get cluster information and the services running
------------------------------------------------

Inside your project directory, run:

.. code-block:: console

   $ hasura cluster status

the output of which looks like:

::

  INFO Reading cluster status...                    
  INFO Status:                                      
  Cluster Name:       h34-archness84-stg
  Cluster Alias:      hasura
  Kube Context:       h34-archness84-stg
  Platform Version:   v0.15.3
  Cluster State:      Synced

  INFO Cluster configuration:                       
  no changes

  INFO Gateway warnings:                            
  none

  INFO Custom services:                             
  NAME      STATUS    ENDPOINT
  adminer   Running   https://adminer.h34-archness84-stg.hasura-app.io

  INFO Hasura services:                             
  NAME            STATUS    ENDPOINT
  auth            Running   https://auth.h34-archness84-stg.hasura-app.io
  data            Running   https://data.h34-archness84-stg.hasura-app.io
  filestore       Running   https://filestore.h34-archness84-stg.hasura-app.io
  gateway         Running   
  le-agent        Running   
  notify          Running   https://notify.h34-archness84-stg.hasura-app.io
  platform-sync   Running   
  postgres        Running   
  session-redis   Running   
  sshd            Running   


The first section shows general information about the cluster.

The cluster configuration section will tell if there is a difference between the local and remote cluster configurations.

The services section shows the custom and Hasura services running in your cluster along with their http endpoint.

Hasura Services
---------------

auth
^^^^
The auth service handles user authentication for your Hasura cluster.

data
^^^^
The data service on Hasura exposes an HTTP/JSON API over a PostgreSQL database.

filestore
^^^^^^^^^
The filestore service lets users upload and store files on the Hasura project and also download when required.

gateway
^^^^^^^
This service exposes your project to the outside world. It authenticates and redirects external requests to the appropriate service inside the cluster.

le-agent
^^^^^^^^
This is the LetsEncrypt agent which is responsible for generating SSL certificates.

notify
^^^^^^
This service handles the Email/SMS communication.

platform-sync
^^^^^^^^^^^^^
This service keeps the cluster in sync with the configuration.

postgres
^^^^^^^^
This service hosts the PostgrSQL database.

session-redis
^^^^^^^^^^^^^
This is a Redis instance used by the auth service.

sshd
^^^^
This service handles SSH access to the cluster.


Next: Start building a blog-engine app!
---------------------------------------

Next, head to :doc:`Part V: Building a blog-engine app <5-build-blog-app>`
