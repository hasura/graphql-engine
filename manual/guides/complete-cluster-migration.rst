Completely migrate a cluster to another cluster
===============================================

This guide will help you push your
existing project including data of the existing cluster into a new one. After
following this you will have a cluster with an exact replica of your source
cluster including data.

.. note::

   The new cluster will have a different domain from the source.


Create a new cluster
--------------------

Create a new  cluster by following :doc:`this <../cluster/create>`.

.. note::

    Ignore this step if you already have a cluster you want to migrate to.

Copy the postgres data
----------------------

Now we have to copy the postgres data, from the source cluster to the new cluster.

The following commands will dump the postgres data from the source cluster,
tar it, copy it into your local filesystem, copy it to the new cluster
and untar it.

.. code-block:: shell

    $ hasura -c <source> ms exec postgres -n hasura -- tar -czf /pg-data.tar.gz /var/lib/postgresql/data
    $ hasura -c <source> ms cp hasura/postgres:/pg-data.tar.gz pg-data.tar.gz
    $ hasura -c <dest> ms cp pg-data.tar.gz hasura/postgres:/pg-data.tar.gz
    $ hasura -c <dest> ms exec postgres -n hasura -- tar -xzf  /pg-data.tar.gz

``<source>`` is the alias for the source cluster, and ``<dest>`` is the alias for the new cluster.

Copy the filestore data
-----------------------

Now we have to copy the filestore data, from the source cluster to the new cluster.

The following commands will dump the filestore data from the source cluster,
tar it, copy it into your local filesystem, copy it to the new cluster
and untar it.

.. code-block:: shell

    $ hasura -c <source> ms exec filestore -n hasura -- tar -czf  /fs-data.tar.gz /var/lib/filestore/data
    $ hasura -c <source> ms cp hasura/filestore:/fs-data.tar.gz fs-data.tar.gz
    $ hasura -c <dest> ms cp fs-data.tar.gz hasura/filestore:/fs-data.tar.gz
    $ hasura -c <dest> ms exec filestore -n hasura -- tar -xzf  /fs-data.tar.gz

``<source>`` is the alias for the source cluster, and ``<dest>`` is the alias for the new cluster.

.. note::

   If you are sure you have not used any filestore features, you can skip this step.


Update the secrets
------------------

Finally, update the secrets from the source to the new cluster.

.. code-block:: shell

    $ hasura -c <source> secrets list # get auth.admin.password and postgres.password
    $ hasura -c <dest> secrets update auth.admin.password <auth-admin-password>
    $ hasura -c <dest> secrets update postgres.password <postgres-password>

``<source>`` is the alias for the source cluster, and ``<dest>`` is the alias for the new cluster.
``<auth-admin-password>`` and ``<postgres-password>`` is obtained from the first command, in this step.

Git push
--------

When all of this data is copied, you can then git push your project to the new
cluster, and it should be live!

.. code-block:: shell

   $ git push <dest> master

where ``<dest>`` is the alias for the new cluster.
