Python
======

Using Gunicorn with Flask
-------------------------

`Gunicorn <http://gunicorn.org/>`_ has a ``reload`` mode which can restart the
server if any of the dependent file changes on disk. Let's see how this works
for the `Flask <http://flask.pocoo.org/>`_ microservice in
`hasura/hello-python-flask
<https://hasura.io/hub/project/hasura/hello-python-flask>`_ quickstart. The
directory structure is as shown below: 

.. code-block:: bash

   .
   ├── Dockerfile
   ├── k8s.yaml
   ├── conf
   │   └── gunicorn_config.py
   └── src
       ├── auth.py
       ├── __init__.py
       ├── requirements.txt
       └── server.py
 
Executing ``gunicorn --reload src:app`` here will start gunicorn in reload mode.
It will restart the server for any change in the python files. Here, we have
kept all the configuration in ``gunicorn_config.py``. Hence we set the reload
parameter in this file:

.. code-block:: python

   reload = True


Let's look at the dockerfile now:

.. code-block:: dockerfile

   # Step 1: use a base python image
   FROM python:3

   # Step 2: install requirements
   COPY src/requirements.txt /tmp/requirements.txt
   RUN pip3 install -r /tmp/requirements.txt
   
   # Step 3: set /app as working directory
   WORKDIR /app
   
   # Step 4: copy current directory to /app
   COPY . /app
   
   # Step 5: run gunicorn server
   # port is configured through the gunicorn config file
   CMD ["gunicorn", "--config", "./conf/gunicorn_config.py", "src:app"]

As per step 4, the current microservice directory is copied into ``/app`` into
the container image. So, after setting reload flag in either step 5 or in
``gunicorn_conf.py`` (note the conf file take precedence over command line
flags), you can execute the following command to sync the ``src`` directory
(assuming microservice is called ``api``):

.. code-block:: bash

   $ hasura microservice sync api microservices/api/src:/app/src

Keep the command running in a terminal. Open the microservice in a browser,
using another terminal window:

.. code-block:: bash

   $ hasura microservice open api

Make some changes to ``server.py`` and save. Refresh the browser and voila! You
can see that changes live in the browser tab.
