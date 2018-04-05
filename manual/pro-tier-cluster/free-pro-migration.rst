Upgrading free clusters
========================

Currently updgrading a free cluster to a pro-tier cluster is a manual process.
We are working on a more seamless experience.

In the meanwhile, this guide will help you to get a new cluster and push your
existing project including data of the existing cluster into the new one. After
following this you will have a new cluster with an exact replica of your free
cluster including data.

.. note::

   This guide creates a new cluster, which means you will get a new domain.


Create a new pro-tier cluster
-------------------------------
To upgrade your free cluster, first create a new pro-tier cluster.
:doc:`More details <./create-pro-tier-cluster>`.

Copy the postgres data
----------------------
Then we have to copy the postgres data, from the free cluster to the new cluster.

The following commands will dump the postgres data from the source cluster (free
cluster), tar it, copy it into your local filesystem, copy it to the new cluster
and untar it.

.. code-block:: shell

    $ hasura -c <source> ms exec postgres -n hasura -- tar -czf /pg-data.tar.gz /var/lib/postgresql/data
    $ hasura -c <source> ms cp hasura/postgres:/pg-data.tar.gz pg-data.tar.gz
    $ hasura -c <dest> ms cp pg-data.tar.gz hasura/postgres:/pg-data.tar.gz
    $ hasura -c <dest> ms exec postgres -n hasura -- tar -xzf  /pg-data.tar.gz

``<source>`` is the alias for the free cluster, and ``<dest>`` is the alias for
the new pro-tier cluster.

Copy the filestore data
-----------------------
Then we have to copy the filestore data, from the free cluster to the new cluster.

The following commands will dump the filestore data from the source cluster (free
cluster), tar it, copy it into your local filesystem, copy it to the new cluster
and untar it.

.. code-block:: shell

    $ hasura -c <source> ms exec filestore -n hasura -- tar -czf  /fs-data.tar.gz /var/lib/filestore/data
    $ hasura -c <source> ms cp hasura/filestore:/fs-data.tar.gz fs-data.tar.gz
    $ hasura -c <dest> ms cp fs-data.tar.gz hasura/filestore:/fs-data.tar.gz
    $ hasura -c <dest> ms exec filestore -n hasura -- tar -xzf  /fs-data.tar.gz

``<source>`` is the alias for the free cluster, and ``<dest>`` is the alias for
the new pro-tier cluster.

.. note::

   If you are sure you have not used any filestore features, you can skip this step.


Update the secrets
------------------

Finally, update the secrets from the free to the new cluster.

.. code-block:: shell

    $ hasura -c <source> secrets list # get auth.admin.password and postgres.password
    $ hasura -c <dest> secrets update auth.admin.password <auth-admin-password>
    $ hasura -c <dest> secrets update postgres.password <postgres-password>

``<source>`` is the alias for the free cluster, and ``<dest>`` is the alias for
the new pro-tier cluster. ``<auth-admin-password>`` and
``<postgres-password>`` is obtained from the first command, in this step.

Git push
--------
When all of this data is copied, you can then git push your project to the new
cluster, and it should be live!

.. code-block:: shell

   $ git push <dest> master

where ``<dest>`` is the alias for the new cluster.
