:orphan:

.. meta::
   :description: Reference documentation for using Hasura's command line tooling, hasuractl
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl

.. _hasuractl-ref:

.. highlight:: bash

Hasuractl
=========

``hasuractl`` is the command line tool for the Hasura platform.

Installation
------------

Linux
~~~~~

Install the latest ``hasuractl`` using the following command:

.. code:: bash

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/linux-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/


If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command


Windows
~~~~~~~

Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/latest/windows-amd64/hasuractl.exe>`_.
and place it in your ``PATH``. Refer to this `video <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_
if you need help with the installation on Windows.

    In Windows, you should only use `git-bash <https://git-scm.com/download/win>`_ to execute commands that you see in this documentation.

Mac OS
~~~~~~

Run the following command to install ``hasuractl``:

.. code::

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/darwin-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command

.. _working-with-hasura:

Working with a trial Hasura project
-----------------------------

1. Download hasuractl.

2. Log into your Hasura account:

.. code:: bash

   $ hasuractl login

3. Initialize your Hasura project:

.. code:: bash

   $ hasuractl init --type=trial

4. Open the API Console

.. code:: bash

   $ hasuractl api-console -c hasura

Quickstart templates
-------------------

Hasura provides readymade quickstart templates for a variety of frameworks.
These can help you get started with developing using these frameworks on the Hasura platform.

To use these quickstart templates, you can run:

.. code:: bash

   $ hasuractl service quickstart app-name --template template-name -c hasura # Replace hasura with your cluster name if you have a different name

To push this, you can do

.. code:: bash

   $ git commit -am "Init"
   $ git push hasura master

Adding your ssh key
-------------------

To add your ssh key, run the following:

.. code:: bash

   $ cat ~/.ssh/id_rsa.pub > clusters/hasura/authorized_keys
   $ hasuractl cluster apply -c hasura

Replace hasura here with your the cluster to which you want to add your ssh key.

Frequently Used Commands
-------------------------

.. _quickstart-cmd:

quickstart
~~~~~~~~~~

These can help you get started with developing using these frameworks on the Hasura platform.

To use these quickstart templates, you can run:

.. code:: bash

   $ hasuractl service quickstart app --template nodejs-express -c hasura # Replace hasura with your cluster name if you have a different name

This will initialize a nodejs-express app in services/app,  initialize a git repository and create a git-push service called app.

.. _forward-cmd:


.. _set-context-cmd:

config set-context
~~~~~~~~~~~~~~~~~~

This command sets the context for all hasuractl commands to a specific cluster, so that the other commands can be used without specifying
a -c option.

.. code::

    $ hasuractl config set-context <cluster-name>
