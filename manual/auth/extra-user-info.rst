.. .. meta::
   :description: Hasura auth users extra profile information
   :keywords: hasura, users, auth, profile, extra info


.. _user-extra-fields:

Storing extra user information
==============================

.. .. todo::
   * Show an example of a profile table with user_id and the permissions

It is a common use-case to store extra information about your users other than
authentication related data to create a proper user profile (like name, age, address etc.).  For this you
should create a custom table in the ``data`` microservice and use that to store the extra user information.

**To store extra information about your users:**

1. Create a table (e.g ``users``) in the data microservice. (refer :doc:`../data/create-table`)
2. Along with all the columns you want to add to the table, add a column called
   ``hasura_id`` of type Integer.
3. Whenever a successful signup happens, enter a row about that user in your
   table. Use the ``hasura_id`` value returned by the Auth API to link the row to the auth user.
