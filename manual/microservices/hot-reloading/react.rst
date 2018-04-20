React
=====

Using create-react-app
----------------------

The popular `create-react-app <https://github.com/facebook/create-react-app>`_
has a script hooked to ``npm start`` which can restart the server when any of
the files is changed. Let's look at a typical file structure:

.. code-block:: bash

   .
   ├── Dockerfile
   ├── k8s.yaml
   └── app
        ├── package.json
        └── src
            ├── App.css
            ├── App.js
            ├── index.css
            └── index.js

``npm start`` can be executed inside app directory, which start a web-server
which also watches for changes. Say ``App.js`` is changed, the script with
reload the web server and the connected website so that new code is available.

.. code-block:: dockerfile
   :caption: Dockerfile

   # Step 1: Pulls a simple ubuntu image with node 8 installed in it
   FROM node:8
   
   # Step 2: Make a new directory called "app"
   RUN mkdir /app
   
   # Step 3: Copy the package.json file from your local directory and paste it inside the container, inside the app directory
   COPY app/package.json /app/package.json
   
   # Step 4: cd into the app directory and run npm install to install application dependencies
   RUN cd /app && npm install 
   
   # Step 5: Add all source code into the app directory from your local app directory
   ADD app /app/
   
   # Step 6: Set app as our current work directory
   WORKDIR /app
   
   # Step 7: Serve the app at port 8080 using the serve package
   ENV PORT 8080
   CMD ["npm", "start"]


The ``app`` directory from the microservice is added as ``/app`` in the docker
image for the container, as given in step 5. The, in step 7, we can see that
``npm start`` is executed. When we commit and push this dockerfile, the
microservice will be running a development webserver that can hot-reload
changes.

The ``sync`` command will keep local ``app`` directory in sync with the one
inside the container for the microservice. As the dockerfile indicates, we start
the sync between the ``app`` directories. Assuming the microservice is called
``ui``:

.. code-block:: bash

   $ hasura microservice sync ui microservices/ui/app:/app

Keep the command running in a terminal. Open the microservice in a browser,
using another terminal window:

.. code-block:: bash

   $ hasura microservice open ui

Make some changes to ``App.js`` and save. Voila! You can see that changes
live in the browser tab.
