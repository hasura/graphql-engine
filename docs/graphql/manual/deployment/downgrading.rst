.. meta::
   :description: Downgrade Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, downgrade, version

Downgrading Hasura GraphQL engine
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


Step 1: Update Hasura GraphQL engine image version
--------------------------------------------------

The Hasura GraphQL engine runs off a Docker image and downgrades are as simple as changing the image tag to the version you want.

Based on your deployment method, follow the appropriate guide to downgrade the GraphQL engine version you're running:

- :doc:`Updating on Heroku <heroku/updating>`
- :doc:`Updating on Docker <docker/updating>`
- :doc:`Updating on Kubernetes <kubernetes/updating>`

If the GraphQL engine version you are downgrading to has a different catalogue version than your current, you will have to downgrade the catalogue
to the corresponding version manually as described below.

Step 2: Downgrade Hasura catalogue version
------------------------------------------

The Hasura GraphQL engine maintains its metadata state in a "catalogue" as described :ref:`here <hasura_metadata_schema>`.
The schema of the catalogue is versioned. Updates to the Hasura GraphQL engine may have Hasura catalogue version bumps.

During upgrades the server automatically migrates the catalogue to the latest version on startup.

Downgrades to the catalogue need to be carried out manually in case you are attempting to downgrade to a lower Hasura GraphQL engine version.

Hasura GraphQL engine versions - Catalogue version mapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::

  Some Hasura GraphQL engine version upgrades might bump the catalogue version by more than 1

+-----------------------------------+---------------------+
| Hasura GraphQL engine versions    | Catalogue version   |
+===================================+=====================+
| v1.0.0-beta.10                    | 27                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.8 - v1.0.0-beta.9     | 26                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.7                     | 24                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.6                     | 22                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.4 - v1.0.0-beta.5     | 19                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.2 - v1.0.0-beta.3     | 17                  |
+-----------------------------------+---------------------+
| v1.0.0-beta.1                     | 16                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha42 - v1.0.0-alpha45   | 13                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha41                    | 12                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha40                    | 11                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha39                    | 10                  |
+-----------------------------------+---------------------+
| v1.0.0-alpha36 - v1.0.0-alpha38   | 9                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha34 - v1.0.0-alpha35   | 7                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha31 - v1.0.0-alpha33   | 6                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha30                    | 5                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha29                    | 4                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha21 - v1.0.0-alpha28   | 3                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha16 - v1.0.0-alpha20   | 2                   |
+-----------------------------------+---------------------+
| v1.0.0-alpha0 - v1.0.0-alpha15    | 1                   |
+-----------------------------------+---------------------+

Downgrading across catalogue versions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can downgrade the catalogue from a particular version to a previous version by executing the ``graphql-engine`` executable on the command line,
with the ``downgrade`` command, specifying the desired catalog version (from the table above) using the ``--to`` option.

.. note::

  - The ``downgrade`` command is not part of the Hasura CLI but rather a command on ``graphql-engine`` itself. The way to execute this command is to 
    run ``docker run hasura/graphql-engine:<VERSION> graphql-engine downgrade --to <NEW-VERSION>``.

  - You need to use a newer version of ``graphql-engine`` to downgrade to an older version, since only the newer version knows how to downgrade from that point in time. After you’ve executed the ``downgrade`` command using the newer 
    version, you should switch to the older version and run ``graphql-engine serve`` as normal.

  - Running this command while Hasura GraphQL engine is running might lead to unexpected results. It is recommended to first bring down any running
    Hasura GraphQL engine instances and then run the SQL statements using an external Postgres client (like ``psql``).

  - Catalogue version downgrades will be executed sequentially and in a single transaction.

.. contents:: Downgrading
  :backlinks: none
  :depth: 1
  :local:

