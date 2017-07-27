:orphan:

.. meta::
   :description: A tutorial on deploying a Django web application using Hasura complete with migrations and a postgres database
   :keywords: hasura, docs, tutorials, python, django, web-application,  migrations, postgres
   :content-tags: python, django, deployment, web-application

Deploying a Django app
===========================


This tutorial will take you over deploying a Django application on Hasura.

Benefits of using Hasura to deploy and host your Django app:
1. A Hasura project comes with a pre-configured Postgres that's ready to be used
2. `git push hasura master` inside your django app will deploy your application to your server
3. Migrations are automatically handled whenever you update and deploy your application to the server!

Basic deployment
----------------
Follow the 4 steps below so that you can start off and deploy a Django app
within minutes. Refer to the next section on :ref:`local development`, to connect to
the Postgres database when you're developing and testing locally.

Step 1: Get a hasura project and set up `hasuractl`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sign up on http://dashboard.hasura.io and get yourself a Hasura project.
Creating a hasura project will give you a domain. Something like: `project42.hasura-app.io`
You will also get an email with your `admin` credentials for your project console and your
database.

.. code::

   Postgres username: admin
   Postgres password: password

Step 2: Install `hasuractl` and initialise a Django project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Initialise a Django project using `hasuractl` as follows

.. code::
    $ hasuractl quickstart python-django <project-name> <hasura-project-name>


This will automatically set up the following file structure:
.. code::
    .
    ├── app
    │   ├── conf
    │   │   ├── gunicorn_config.py
    │   │   └── requirements.txt
    │   └── src
    │       └── helloworld
    │           ├── helloworld
    │           │   ├── __init__.py
    │           │   ├── settings.py
    │           │   ├── urls.py
    │           │   ├── views.py
    │           │   └── wsgi.py
    │           └── manage.py
    ├── docker-config.yaml
    ├── Dockerfile
    └── README.md


You can also remove the helloworld folder and create your own Django project in
its place using 

.. code::

    $ django-admin startproject <project-name>


Once you create a new project, make sure to edit the Docker file and replace
the `helloworld` in the following line near the end of the file

.. code::
    
    WORKDIR /usr/src/app/helloworld

with your project name 
.. code::

    WORKDIR /usr/src/app/<project-name>


Step 3: Configure your Django `settings.py` file and set the postgres password
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the postgres password from Step 1 above, and set that in your `.env` file.
Edit your django settings.py file to get the database credentials from
environment variables.

Replace the Database section of your settings.py file with the following
.. code::

    DATABASES = {
        'default': {
            'ENGINE': 'django.db.backends.postgresql_psycopg2',
            'NAME': 'hasuradb',
            'USER': 'admin',
            'PASSWORD': os.environ.get('POSTGRES_PASSWORD'),
            'HOST': os.environ.get('POSTGRES_HOST'),
            'PORT': '5432',
        }
    }

This ensures that your app uses the correct database credentials.

Now you will need to create a Custom Microservice on the Hasura console, to
which you can deploy your Django project. You can follow the instructions at
the `Hasura documentation <https://docs.hasura.io/0.13/ref/custom-microservices/index.html>`_ in order to do this.

You will also need to set these environment variables in the hasura console for your
git-push service so that your app will have access to them when deployed. You
can do this at
``console.<hasura-project-name>.hasura-app.io/gitpush/<git-push-service-name>/update``
.. code::

    POSTGRES_PASSWORD :  <postgres-password-from-email>
    POSTGRES_HOST : postgres.hasura

Also remember to edit your Available Hosts to look as follows

.. code::

    ALLOWED_HOSTS = ['*']

With this we've finished configuring our project to work with Hasura.

Step 3: Use hasuractl to add your SSH key to the Hasura project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You need to add your ssh key to the Hasura project in order to deploy your
project.  In case you do not have an ssh-key,  you can create one following the
instructions `here <https://confluence.atlassian.com/bitbucketserver/creating-ssh-keys-776639788.html>`_.


.. code::

    $ hasuractl add-ssh-key

Step 4: `git push` and you're done!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To deploy your code, now just do 

.. code::

    $ git push hasura master

Voila, your service is deployed and live! In case there are any errors in building or deploying your code,
the ``git push`` command will show you errors and the push will fail. Fix the error, and push again!

.. _local-development:
Local development
-----------------

Considering that the postgres database is already on the Hasura project cluster, when you are
developing on your own mahcine, on your application you might want to connect to the database too.

Step 1: Setup a secure tunnel to your database
----------------------------------------------

You can use the following command to set up an ssh tunnel in order to access
postgres locally.

.. code::

    $ hasuractl forward 5432:postgres.hasura:5432
 

Step 2: Run your app locally! 
------------------------------------------------

Now use manage.py runserver in your project directory to run your app locally!

.. code::

    $ POSTGRES_PASSWORD='<password>' POSTGRES_HOST='localhost' python3
    manage.py runserver

Go to ``127.0.0.1:8000`` to see your app live!
