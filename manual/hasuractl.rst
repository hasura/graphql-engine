:orphan:

.. meta::
   :description: Reference documentation for using Hasura's command line tooling, hasuractl
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl

.. _hasuractl:

.. highlight:: bash

hasuractl
=========

``hasuractl`` is the command line tool for the Hasura platform.

Starting v0.15, ``hasuractl`` is the primary mode of managing Hasura projects and Hasura clusters.

.. _hasuractl-installation

Installation
------------

.. note::

   ``git`` is a dependency for ``hasuractl``. Make sure that git is installed in your system. 

Linux
~~~~~

Install the latest ``hasuractl`` using the following command:

.. code:: bash

    # curl 
    $ curl -L https://hasura.io/install.sh | bash 
    # OR
    # wget
    $ wget -qO- https://hasura.io/install.sh | bash


    # This command will download a bash script and execute it, which will in turn download the latest version of `hasuractl` and install it into `/usr/local/bin`. You will be prompted for the root password to complete installation.

Windows
~~~~~~~

Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/latest/windows-amd64/hasuractl.exe>`_.
and place it in your ``PATH``. Refer to this `video <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_
if you need help with the installation on Windows.

.. note::

    It is recommended to use `git-bash <https://git-scm.com/download/win>`_ on Windows to execute hasuractl commands.

Mac OS
~~~~~~

Run the following command to install ``hasuractl``:

.. code:: bash

    # curl 
    $ curl -L https://hasura.io/install.sh | bash 
    # OR
    # wget
    $ wget -qO- https://hasura.io/install.sh | bash


    # This command will download a bash script and execute it, which will in turn download the latest version of `hasuractl` and install it into `/usr/local/bin`. You will be prompted for the root password to complete installation.

.. _hasuractl-getting-started

Getting started
---------------

Create a project and cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are new to ``hasuractl`` and have not created any projects or clusters before, continue. Otherwise jump to <another-location>.

1. Install ``hasuractl``
2. Login using your Hasura account:

.. code:: bash

   $ hasuractl login

3. Create a Hasura project and get a trial cluster, also change to the project directory:

.. code:: bash

   $ hasuractl create my-project --type=trial 
   $ cd my-project 

   # This command creates a directory called my-project, creates a trial cluster called `hasura` for you, adds it to the project and sets it as default.

4. Open the API console:

.. code:: bash

   $ hasuractl api-console 
          
Using the API console, you can try out Hasura APIs for Auth, Data, File and Notify. You can also create and manage tables for your database, see users in your cluster etc.

Deploy custom code
~~~~~~~~~~~~~~~~~~

For hosting your own code or static HTML websites, Hasura provides ready-made quickstart templates for a variety of frameworks. Find all the quickstart templates `here <https://github.com/hasura/quickstart-docker-git>`_

You can add a template along with it's source code to your newly created Hasura project and deploy it to the cluster. These templates are deployed as micro-services on Hasura platform. Changes to the source code can be re-deployed using ``git push``.

1. Initialize a git repo inside your project

.. code:: bash

   $ git init 

2. Add the service and create it on the cluster:

.. code:: bash

   # for e.g., deploy a Python Flask based web server, name it api
   $ hasuractl service quickstart api --template python-flask

   # This command downloads the template, copies it into ``services`` directory in the project, creates this service on the cluster, adds a URL route for it, adds your SSH key to the cluster, creates a git remote for you to push and creates an initial commit for the code.

3. Deploy the code

.. code:: bash

   $ git push hasura master

   # Your service will be live at https://api.<cluster-name>.hasura-app.io

4. Deploy changes

Make changes to the source code in ``service/python-flask`` directory, commit them and push again:

.. code:: bash

   $ git add <files>
   $ git commit -m "<commit-message>"
   $ git push hasura master

.. note::

   You can find all the available quickstart templates here: `https://github.com/hasura/quickstart-docker-git <https://github.com/hasura/quickstart-docker-git>`_
