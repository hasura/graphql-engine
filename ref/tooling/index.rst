.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Advanced tooling (kubectl, postgres)
====================================

In this section, we will look at how we can access the underlying components of
a Hasura project (like Kubernetes, Postgres etc.).

Kubernetes
----------

To access the underlying Kubernetes objects, you need to install ``kubectl``.

You can go to http://kubernetes.io/docs/user-guide/prereqs/ and download
``kubectl`` from there.

Once you have ``kubectl``, you can follow the instructions on the "Advanced
Settings" page of your project console to setup ``kubectl`` to talk to the
Kubernetes server.


PostgreSQL
----------

Currently, TCP services (like Postgres) can't be exposed on the gateway. However, we have an SSH service that allows you to securely setup tunnels to TCP services for direct access.

To access Postgres, first you have to create SSH tunnel. For example:

.. code-block:: console

  $ ssh -p 22 -L 5432:postgres.default:5432 hasura@ssh.test42.hasura-app.io

This will create a SSH tunnel from your local machine to
``ssh.test42.hasura-app.io``; and will forward your machine's port 5432 to
``postgres.default:5432``. The ``postgres.default:5432`` is the internal
hostname + port of the Postgres instance (as seen by the
``ssh.test42.hasura-app.io`` service).

.. note::
   The above command is an example.  You can go to the "Advanced Settings" page
   in your project console, to get the exact command to setup the SSH tunnel.

Once the tunnel is created, then you can run ``psql`` as if you are connecting
to your local machine's Postgres.

.. code-block:: console

  $ psql -U admin -h localhost

This will prompt you to enter the postgres password, which is available in the
credentials email.

.. note::
  The postgres username and password is stored as Kubernetes secret. Using
  kubectl you can access these secrets.

  All user data is stored in database: "hasuradb".
