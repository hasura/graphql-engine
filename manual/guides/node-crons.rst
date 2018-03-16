.. _node-cron

Adding a nodejs cron job to your Hasura Project
===============================================

`hasura/nodejs-cron <https://hasura.io/hub/projects/hasura/nodejs-cron>`_ is a quickstart that you can find on Hasura Hub.

It consists of a simple nodejs-cron job running which prints something to the console based on the specified time interval.

The name of the microservice is ``cron``, to get this cron job in your project:

.. code-block:: bash

   # ensure that you are inside your project directory
   hasura microservice clone cron --from hasura/nodejs-cron

You will now have this microservice inside your ``microservices/cron`` directory. You can find the source code for the cron job inside ``microservices/cron/src/server.js``

Next, we have to ensure that HasuraCtl knows that this microservice needs to be `git pushed`. To do this, we need to add configuration to your conf/ci.yaml file so that git push hasura master will automatically deploy your source code, build the docker image, and rollout the update!

.. code-block:: bash

  $ hasura conf generate-remote cron >> conf/ci.yaml
  
Since this is a microservice running a cron, it can be a headless microservice which is not exposed via an external URL. 


Optionally, if you do want to expose the microservice via an external URL

.. code-block:: bash

  $ hasura conf generate-route cron >> conf/routes.yaml


To deploy this microservice to your cluster:

.. code-block:: bash

  # Ensure that you are inside your project directory
  git add microservices/cron && git commit -m "Added nodejs cron job"
  git push hasura master
