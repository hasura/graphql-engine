NodeJS
======

Using nodemon
-------------

NodeJS applications can be restarted when a source file changes using tools like
`nodemon <https://nodemon.io/>`_. Let's look at an example microservice, with
the following directory service:

.. code-block:: bash

   .
   ├── Dockerfile
   ├── k8s.yaml
   └── src
       ├── package.json
       └── server.js

A typical local development workflow would be to execute ``nodemon server.js``
inside ``src`` directory. ``nodemon`` will start watching the files and runs the
web-server. Whenever you edit ``server.js`` or any other ``js`` files, nodemon
will reload the server with latest code.

We can replicate the same workflow on a Hasura cluster also. Let's see how we
can modify the typical ``Dockerfile`` for the above microservice to include
``nodemon`` and enable hot reloading.

.. code-block:: dockerfile

   # Step 1: Pulls a simple ubuntu image with node 8 installed in it
   FROM node:8
   
   # Step 2: install nodemon
   RUN npm install -g nodemon

   # Step 3: Make a new directory called "src"
   RUN mkdir /src
   
   # Step 4: Copy the package.json file from your local directory and paste it inside the container, inside the src directory
   COPY src/package.json /src/package.json
   
   # Step 5: cd into the src directory and run npm install to install application dependencies
   RUN cd /src && npm install
   
   # Step 6: Add all source code into the src directory from your local src directory
   ADD src /src
   
   # Step 7: Set src as our current work directory
   WORKDIR /src
   
   # Step 8: run the nodemon command
   CMD ["nodemon", "server.js"]

As per step 6, we are copying ``src`` directory from local system into
``/src`` of the container. Also, in step 8, we are starting a server using
nodemon. Once you commit and push this dockerfile, the microservice is running
nodemon and will restart for any changes to ``server.js`` and related files.

Next step is to start the ``sync`` command which continuously copies the
required files to the container as and when they change. As we mentioned
earlier, the dockerfile indicates local ``src`` directory is copied over as
``/src``. So, we execute the following command (assuming microservice name is
api):

.. code-block:: bash

   $ hasura microservice sync api microservices/api/src:/src

Keep the command running in a terminal. Open the microservice in a browser,
using another terminal window:

.. code-block:: bash

   $ hasura microservice open api

Make some changes to ``server.js`` and save. Voila! You can see that changes
live in the browser tab.
