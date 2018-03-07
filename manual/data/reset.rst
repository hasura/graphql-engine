Reset database
==============

Sometimes you might want to delete all the data from the database and start from scratch. You can do that by the ``db-reset`` command.

  You will not be able to get back your data. Do it only if you are absolutely sure.

.. code:: bash

  $ hasura migration db-reset -c <cluster-alias>

The above command will clear the data from the database. If you want to clear the schema migrations, remove them from the migrations directory.

.. code:: bash

  $ rm -rf migrations/*