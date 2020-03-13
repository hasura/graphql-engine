.. _setup_migrations:

Setting up Hasura migrations
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Initializing migrations
-----------------------

.. code-block:: bash

  hasura init

  hasura migrate create --from-server <endpoint>
  hasura metadata export --from-server <endpoint>

Generating migrations
---------------------

- Create migrations

  .. rst-class:: api_tabs
  .. tabs::

    .. tab:: Via console

      - Open console

        .. code-block:: bash

          hasura console

      - Do things
      - Migration created and metadata updated automatically

    .. tab:: Manually

      - Create migration folder

        .. code-block:: bash

          hasura migrate create <name-of-migration>

      - Add SQL manually
      - Edit metadata manually

- Squash migrations

  .. code-block:: bash

    hasura migrate squash --from <version>

- Add checkpoint

  .. code-block:: bash

    git commit

Applying migrations
-------------------

- Get migrations directory and metadata.yaml

- Apply migrations

  .. code-block:: bash

    hasura migrate apply
    hasura metadata apply