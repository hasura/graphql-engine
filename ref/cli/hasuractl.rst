:orphan:

.. meta::
   :description: Reference documentation for using Hasura's command line tooling, HasuraCTL
   :keywords: hasura, docs, CLI, HasuraCTL

.. _hasuractl:

HasuraCTL
=========

``hasuractl`` is the commandline tool for the Hasura platform. 

Installation
------------

* Install latest ``kubectl`` (>= 1.6.0) (https://kubernetes.io/docs/tasks/kubectl/install/)


* Install ``hasuractl`` on Windows:

    Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/v0.1.7/windows-amd64/hasuractl.exe>`_ and place it in your ``PATH``. Refer to this `video reference <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_ if you need help with the installation on Windows.

* Install ``hasuractl`` on Linux:

.. code::

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/v0.1.7/linux-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

Feel free to leave off the ``sudo mv hasuractl /usr/local/bin`` if you would like to add hasuractl to your path manually

* Install ``hasuractl`` on Mac:

.. code::

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/v0.1.7/darwin-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

Feel free to leave off the ``sudo mv hasuractl /usr/local/bin`` if you would like to add hasuractl to your path manually

NOTE:
-----

- If you are on windows, you should only use git-bash to execute commands that you see in this documentation.
- If you already have hasuractl installed, replace the old binary with the new one.

Starting hasura
===============

1. Create an account on `beta.hasura.io <https://beta.hasura.io>`_ if you do not have one.

2. Run:

.. code::

   $ hasuractl login

3. Set context: 

   You will need to create a Hasura project for the next step. You can do that
   at `dashboard.hasura.io <https://dashboard.hasura.io/projects>`_.

   Set your project context using 

.. code::

   $ hasuractl set-context <project-name>

Here <project-name> is the name of the project you created on the Hasura
Dashboard. This command also sets the Kubernetes context.
