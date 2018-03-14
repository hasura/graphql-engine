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

To deploy this microservice to your cluster:

.. code-block:: bash

  # Ensure that you are inside your project directory
  git add microservices/cron && git commit -m "Added nodejs cron job"
  git push hasura master
