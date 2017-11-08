.. .. meta::
   :description: Hasura auth users extra profile information
   :keywords: hasura, users, auth, profile, extra info


.. _user-extra-fields:

Extra user profile information
===============================

.. todo::

   * Make language more positive
   * Show an example of a profile table with user_id and the permissions

It is a common use-case to store extra information about your users other than
authentication related data (like name, age, address etc.). But Hasura Auth
APIs does not have provision to store any extra information. Hence you would
have to store it in your own table(s).

To store extra information about your users:

1. Create a table (e.g ``users``) in Hasura DB. (refer TODO)
2. Along with all the columns you want to add to the table, add a column called
   ``hasura_id`` of type Integer.
3. Whenever a successful signup happens, enter a row about that user in your
   table. Use the ``hasura_id`` returned by the Auth API. 
