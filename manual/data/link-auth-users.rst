.. .. meta::
   :description: Linking users to auth
   :keywords: hasura, docs, postgres, tunnel

Linking data to auth users
==========================

It is a common requirement to link data in a table with users maintained by the Auth microservice.

This can be achieved by adding a new field called ``user_id`` (or any other name) of type ``integer`` to the table and storing the ``hasura_id`` of the user maintained by the Auth microservice in it.

The ``user_id`` field can be used to define **ownership** of rows in a table. ie: limit users to fetch/edit their own data only. See :doc:`permissions <permissions>` for more details.

For a logged in user, the ``hasura_id`` can be obtained like :doc:`this <../auth/user-actions/user-info>`
