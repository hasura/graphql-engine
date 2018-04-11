.. _api_explorer:

Hasura API Console
==================

.. image:: img/console-screenshot.png

The Hasura API console is an easy-to-use UI that lets you explore the Data, Auth, Filestore and Notify Hasura APIs on a cluster and also lets you manage the data & configuration of these microservices.

To open the API console, ``cd`` into the project directory and run:

.. code-block:: bash

  $ hasura api-console # optionally -c <cluster-alias>

This will run a local server on your system and will open up the api console in a browser window at ``http://localhost:9695``. By default, the API explorer section of the console will be open.

Here is a quick overview of the prominent features of the API-Console:

.. toctree::
   :maxdepth: 2

    API-Explorer <api-explorer>
    Data <data>
    Auth <auth>
    Filestore <filestore>
    Notify <notify>
