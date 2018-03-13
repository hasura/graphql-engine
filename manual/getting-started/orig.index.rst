.. .. meta::
   :description: Getting started with Hasura
   :keywords: hasura, quickstart, getting started, installation


.. _getting-started-orig:

Getting started
===============

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

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         Download the ``hasura.exe`` binary from here: `hasura.exe Windows <https://dl.equinox.io/tanmai-gopal/hasuractl/stable>`_

         Next, add it to your ``PATH`` so that you can use it from your *command-prompt* or your ``git-bash``.



Step 2: Login to your Hasura account
------------------------------------
Next, run the command below to login or create a new Hasura account.

.. code-block:: bash

  hasura login

A browser window will open up for you to login/register.

Step 3: Create a hasura project based on a starter kit
------------------------------------------------------

.. code-block:: bash

   hasura quickstart hello-world

This creates a folder in your current folder with the above name and
initialises a Hasura project.

You can browse a list of community contributed starter kits at: `hasura.io/hub <https://hasura.io/hub>`_

*Note: The first time you run the quickstart command, your free cluster also gets created.*

Step 4: Deploy your hasura project on your free cluster
-------------------------------------------------------

``hasura quickstart`` automatically initialises a git repo in the above
folder.

Next commit and push.

.. code-block:: bash

  git add .
  git commit -m "Initial commit" && git push hasura master


Step 5: Browse and explore your backend APIs
--------------------------------------------

Running the following command will open `localhost:8080 <http://localhost:8080>`_ to open the API console
and browse your APIs.

.. code-block:: bash

   hasura api-console

**Note**: If you have something running on port 8080, you can pass the
``--console-port`` flag to specify some other port.

.. code-block:: bash

   hasura api-console --console-port 3000
