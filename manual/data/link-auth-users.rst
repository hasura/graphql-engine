.. .. meta::
   :description: Linking users to auth
   :keywords: hasura, docs, postgres, tunnel

Linking data to auth users
==========================

The :doc:`Auth microservice <../auth/index>` assigns a unique integer identifier called ``hasura_id`` to every user registered on it.

It is a common requirement to link data in a table with users maintained by the Auth microservice.
This can be achieved by adding a new field called ``user_id`` (or any other name of your choice) of type ``integer`` to the table and storing the ``hasura_id`` of the user in it while adding a row.

For a logged in user, the ``hasura_id`` can be obtained like :doc:`this <../auth/user-actions/user-info>`

.. note::
   The ``user_id`` field can now be used to define ownership of the rows in the table. ie: limit users to fetch/edit their own data only. See :doc:`data permissions <permissions>` for more details.
