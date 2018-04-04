Hot reloading
=============

Using Hasura, you can hot reload code changes to microservices running on a cluster. If the language/framework tooling supports live reloading, you can do the same on a Hasura cluster so that your changes are deployed live to a HTTPS url.

The Hasura CLI tool can watch a directory for a microservice and update the files inside the microservice container running in the cluster as and when changes happen on the local system. This combined with the hot-reloading capabilities of language/framework, can provide immediate deployment of the code.

Below are couple of examples for various languages:

NodeJS using nodemon
--------------------

NodeJS applications can be restarted when a source file changes using tools like `nodemon <https://nodemon.io/>`_. The dockerfile can be modified as shown below to enable nodemon.

.. code-block:: dockerfile
   :caption: Dockerfile

   ...
   
   # install nodemon
   RUN npm install -g nodemon

   # run the command
   CMD ["nodemon", "server.js"]

Once the dockerfile is changed and pushed, you can execute the following command to start the watch and sync process (assuming microservice name is ``api`` and the source code inside ``microservice/api/src`` directory locally and at ``/src`` inside the container):

.. code-block:: bash

   $ hasura microservice sync microservices/api/src api:/src

   # open the microservice in a browser to see changes:
   $ hasura microservice open api


React using create-react-app
----------------------------

`create-react-app <https://github.com/facebook/create-react-app>`_ has a script hooked to ``npm start`` which can restart the server when any of the files is changed.

.. code-block:: dockerfile
   :caption: Dockerfile

   ...

   # run the server
   CMD ["npm", "start"]

Once this change is pushed, ``sync`` command can be executed to watch the directory and reload automatically:

.. code-block:: bash

   $ hasura microservice sync ui microservices/ui/app/src:/app/src

   # open the microservice in a browser to see changes:
   $ hasura microservice open api

   # make some edit and the tab reloads automatically

For more details, read the full CLI reference for :doc:`hasura microservice sync <../hasuractl/hasura_microservice_sync>`.
