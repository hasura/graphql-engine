Securing the GraphQL endpoint (Heroku)
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.


Add the HASURA_GRAPHQL_ADMIN_SECRET env var
-----------------------------------------

Head to the config-vars URL on your Heroku dashboard and set the ``HASURA_GRAPHQL_ADMIN_SECRET`` environment variable.

.. image:: ../../../../img/graphql/manual/deployment/secure-heroku.png

Setting this environment variable will automatically restart the dyno. Now when you access your console, you'll be
prompted for the admin secret key.

.. image:: ../../../../img/graphql/manual/deployment/access-key-console.png


(optional) Use the admin secret with the CLI
------------------------------------------

In case you're using the CLI to open the Hasura console, use the ``admin-secret`` flag when you open the console:

.. code-block:: bash

   hasura console --admin-secret=myadminsecretkey


.. note::

  If you're looking at adding access control rules for your data to your GraphQL API then head
  to :doc:`Authentication / access control <../auth/index>`.
