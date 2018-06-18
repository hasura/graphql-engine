Adding/updating project secrets
===============================

You can add new or modify old secrets on a cluster by using this command:

.. code-block:: bash

   $ hasura secrets update <my.secret.key> <some-secret-value>   # optionally -c <cluster-alias>

Here ``<my.secret.key>`` is the name of the secret and ``<some-secret-value>`` is the value of the secret.