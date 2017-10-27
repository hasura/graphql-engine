.. meta::
   :description: Getting started with Hasura
   :keywords: hasura, quickstart, hello world, installation


Getting started with Hasura
===========================

..
   Hasura helps you create clusters on which you can deploy your backend quickly.
   - Step 1: Install the hasura CLI tool
   - Step 2: Create a hasura project based off a starter kit of your choice
   - Step 3: ``git push hasura master`` to deploy your backend to your free cluster
   - Step 4: Browse your backend APIs: ``hasura api-console``

Step 1: Install the hasura CLI tool
-----------------------------------

.. tabs::

   tabs:
     - id: linux
       content: |
         Open your linux shell and run the following command:

         .. code-block:: bash

            curl https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         Next, run the command below to login or create a new Hasura account.

         .. code-block:: bash

            hasura login
            #A browser window will open up for you to login/register

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         Next, run the command below to login or create a new Hasura account.

         .. code-block:: bash

            hasura login
            #A browser window will open up for you to login/register


     - id: windows
       content: |

         Download the ``hasura.exe`` binary from here: `hasura.exe Windows <https://dl.equinox.io/tanmai-gopal/hasuractl/stable>`_

         Next, add it to your ``PATH`` so that you can use it from your *command-prompt* or your ``git-bash``.

         Finally, run the command below to login or create a new Hasura account.

         .. code-block:: bash

            hasura.exe login
            #A browser window will open up for you to login/register


Step 2: Create a hasura project based on a starter kit
------------------------------------------------------

.. code-block:: bash

   hasura quickstart hasura/hello-world

You can browse a list of community contributed starter kits at: `hub.hasura.io <https://hub.hasura.io>`_

.. note::

   The first time you run the clone command your free backend cluster also gets created.

Step 3: Deploy your hasura project on your free cluster
-------------------------------------------------------

.. code-block:: bash

   git push hasura master

Step 4: Browse and explore your backend APIs
--------------------------------------------

Running the following command will open `localhost:8080 <http://localhost:8080>`_ to open the API console
and browse your APIs.

.. code-block:: bash

   hasura api-console
