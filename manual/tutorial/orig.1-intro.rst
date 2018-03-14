.. .. meta::
   :description: Part 1 of a set of learning exercises meant for exploring Hasura in detail. This pre-requisite part deals with creating a Hasura project.
   :keywords: hasura, getting started, step 1

============================
Part I: Setup the Hasura CLI
============================

The ``hasura`` CLI is a command line utility to help you get your backend setup quickly. It helps you create projects, manage clusters and manage microservices and explore APIs running on the cluster.

.. note::

   ``hasura`` works using a CLI experience only so that you can maintain all the files that contain your configuration,
   source code and database schema information on your filesystem directly. This makes it easy for you to collaborate using
   tools like git.

Step 1: Install the hasura CLI tool
-----------------------------------

.. tabs::

   tabs:
     - id: linux
       content: |
         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         Note: You need to be running 64-bit windows to run the ``hasura`` CLI.
         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://storage.googleapis.com/hasuractl/v0.2.1/windows-amd64/hasura.msi>`_


Step 2: Login
-------------

Next, run the command below to login or create a new Hasura account.

.. code-block:: bash

  hasura login

A browser window will open up for you to login or register.
Once you've signed in, you'll see a 'Login success' message on the browser.
Close the browser tab, and return to the terminal.

You have now created an account or logged in to your account on hasura.io.
You can also view your account profile and other details on
`dashboard.hasura.io <https://dashboard.hasura.io>`_. You will NOT need to use the 'dashboard'
for the remainder of this tutorial and you can ignore it for now.


Terminology
-----------

Project name
^^^^^^^^^^^^

Typically a word followed by an number; randomly generated when you create a project.
Eg: ``test42``. It'll be referred to as ``<cluster-name>``.

Project domain
^^^^^^^^^^^^^^

The domain which will resolve to your hasura project. By default, it is ``<cluster-name>.hasura-app.io`` or ``hasura.test`` (local).

Services
^^^^^^^^

A microservice is an abstract entity which captures a running web/tcp server. There are several microservices that come out of the box with hasura platform like ``data``, ``auth`` and ``console``.

An HTTP microservice, say ``svc`` is typically exposed using a subdomain at ``<svc>.<cluster-name>.hasura-app.io``. For example, the ``data`` microservice is exposed at ``data.<cluster-name>.hasura-app.io``.

Gateway
^^^^^^^

The Gateway is the entrypoint for your project. Every request to the project goes through the gateway and is then routed to the appropriate microservice. For example, all requests on ``data.<cluster-name>.hasura-app.io`` are forwarded to the built-in hasura provided ``data`` microservice.

Console
^^^^^^^

The UI microservice that helps you manage data, users and other microservices in your project. It can be accessed at ``console.<cluster-name>.hasura-app.io``

Admin user
^^^^^^^^^^

The ``admin`` user is a special user who can manage the project. The credentials for the ``admin`` user are sent in an email after the project creation is completed. In case of local setup, the password for the admin user is ``password``. You can use these credentials to login to the console.

.. note:: Every project gets its own ``admin`` user. `Hasura Project Dashboard`_ is the dashboard where you can create multiple projects. Login credentials that are generated for your project (and sent to you via email) have nothing to do with the login credentials of `Hasura Project Dashboard`_

Admin token
^^^^^^^^^^^

When you login to the console with the ``admin`` user credentials, you can see the admin token listed under the "Project Info" section.
