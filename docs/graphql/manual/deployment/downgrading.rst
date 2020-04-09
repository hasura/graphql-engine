.. meta::
   :description: Downgrade Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, downgrade, version

.. _downgrade_hge:

Downgrading Hasura GraphQL engine
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


Step 1: Update Hasura GraphQL engine image version
--------------------------------------------------

The Hasura GraphQL engine runs off a Docker image and downgrades are as simple
as changing the image tag to the version you want.

Based on your deployment method, follow the appropriate guide to downgrade the
GraphQL engine version you're running:

- :ref:`Updating on Heroku <heroku_update>`
- :ref:`Updating on Docker <docker_update>`
- :ref:`Updating on Kubernetes <kubernetes_update>`

If the GraphQL engine version you are downgrading to has a different catalogue
version than your current, you will have to downgrade the catalogue
to the corresponding version manually as described below.

Step 2: Downgrade Hasura catalogue version
------------------------------------------

The Hasura GraphQL engine maintains its metadata state in a "catalogue" as
described :ref:`here <hasura_metadata_schema>`. The schema of the catalogue is
versioned. Updates to the Hasura GraphQL engine may have Hasura catalogue
version bumps.

Downgrades to the catalogue need to be carried out manually in case you are
attempting to downgrade to a lower Hasura GraphQL engine version.

From ``v1.2.0``, you can downgrade the catalogue from a particular version to a
previous version by executing the ``graphql-engine`` executable on the command
line, with the ``downgrade`` command, specifying the desired catalogue version
using one of the ``--to-`` flags. For earlier versions, it is recommended to
first upgrade to the latest version and then use the ``downgrade`` command to
downgrade to the desired version.

The ``downgrade`` command is not part of the Hasura CLI but rather a command on
``graphql-engine`` itself. The way to execute this command is to run:

.. code-block:: bash

  docker run hasura/graphql-engine:<VERSION> graphql-engine downgrade --to-<NEW-VERSION>

You need to use a newer version of ``graphql-engine`` to downgrade to an older
version, since only the newer version knows how to downgrade from that point in
time. After you’ve executed the ``downgrade`` command using the newer version,
you should switch to the older version and run ``graphql-engine serve`` as normal.

Catalogue version downgrades will be executed sequentially and in a single
transaction.

.. note::

  Running this command while Hasura GraphQL engine is running might lead to
  unexpected results. It is recommended to first bring down any running
  Hasura GraphQL engine instances before downgrading the catalogue
