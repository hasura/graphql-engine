.. meta::
   :description: MySQL preview
   :keywords: hasura, mysql, preview

.. _mysql_preview:

MySQL preview
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

We are in the process of launching support for MySQL, and we have a preview available for you to try.

.. admonition:: Supported MySQL versions

  Hasura GraphQL engine currently supports **MySQL 8.0.14 and above**.

Try it out
----------

MySQL support can be tried out using ``docker-compose`` and an existing MySQL database
as follows:

Prerequisites
^^^^^^^^^^^^^

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`_
- An existing MySQL database

Step 1: Get the docker-compose file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Get the Hasura MySQL preview docker compose file:

.. code-block:: bash

   # in a new directory run
   wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-mysql-preview/docker-compose.yaml
   # or run
   curl https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-mysql-preview/docker-compose.yaml -o docker-compose.yaml

Step 2: Update MySQL config
^^^^^^^^^^^^^^^^^^^^^^^^^^^

You need to now update the ``docker-compose.yaml`` file with credentials for your
MySQL database *(replace the values surrounded by <>)*

.. code-block:: yaml

   ...
   command:
    - graphql-engine
    - --mysql-host
    - <mysql-host>
    - --mysql-user
    - <mysql-user>
    - --mysql-port
    - <mysql-port>
    - --mysql-dbname
    - <mysql-dbname>
    - --mysql-password
    - <mysql-password>
    ...

Our :ref:`docker networking guide <docker_networking>` might be useful to set
the appropriate value for ``mysql-host``. *(See Hasura to API)*

Step 3: Run Hasura GraphQL engine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following command will run Hasura along with a Postgres database required
for its functioning.

.. code-block::  bash

   $ docker-compose up -d

Check if the containers are running:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

Step 4: Try out the GraphQL API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GraphiQL on the Hasura console available at ``http://localhost:8080/console`` can be
used to try out the generated GraphQL API.

**The Hasura console currently does not support managing the MySQL database schema**.
i.e. The ``Data`` section of the console will not display the MySQL tables, etc. Hence the database
schema needs to be managed externally as of now. *(support for this is coming very soon)*

See the `source PR <https://github.com/hasura/graphql-engine/pull/5655>`__ for more information
on current limitations and upcoming features.

Keep up to date
---------------

If you'd like to stay informed about the status of MySQL support, subscribe here:

.. raw:: html

    <div>
      <div id="mysql_embed_signup" class="mc_embed_signup">
        <form id="mysql-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate post-subscription-form mc-embedded-subscribe-form mysql-subscribe-form" target="_blank" rel="noopener" novalidate>
          <div style="width: 40%">
            <div class="input-box">
              <input type="email" name="EMAIL" id="mysql-EMAIL" class="mce-EMAIL" aria-label="Email" placeholder="Your Email Address" pattern="^([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22))*\x40([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d))*(\.\w{2,})+$" required>
            </div>
            <div id="mysql-responses" class="clear display-inline mce-responses">
              <div id="mysql-error-response" class="mce-error-response response error-message hide">
              </div>
              <div id="mysql-success-response" class="mce-success-response response success-message hide">
              </div>
            </div>
          </div>
          <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_9b63e92a98ecdc99732456b0e_f5c4f66bcf" tabindex="-1" value=""></div>
          <div class="clear submit-box" style="max-width: 120px !important">
              <input type="submit" disabled="true" value="Subscribe" name="subscribe" id="mysql-embedded-subscribe" class="button mc-embedded-subscribe">
          </div>
        </form>
      </div>
    </div>

Give us feedback
----------------

We appreciate any feedback. Please open a new `GitHub discussion <https://github.com/hasura/graphql-engine/discussions>`__, and we can discuss there.
