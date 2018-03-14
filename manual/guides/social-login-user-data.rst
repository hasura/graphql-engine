.. _social-login-user-data

Integrating social login with user table
========================================

Sometimes while performing social login with Hasura, you would want to store the information returned by the login provider in the database to give the user some form of personalized experience.
Let's a take an example to see how this can be done to store the user's ``access token`` so that you can use it to fetch their friends list or other information from ``facebook``.

To do this:

1. Create a new table, let's call it ``user_details`` with the following columns to it, ``user_id`` of type ``INTEGER`` and  ``access_token`` of type ``TEXT``. Let ``user_id`` be the primary key as theyre unique for each user.
2. Add user permissions to this table. The ``user_id`` field can be used to define **ownership** of rows in a table. ie: limit users to fetch/edit their own data only. See :doc:`permissions <../data/permissions>` for more details.
3. Once you complete social login with Hasura, make an insert data query into the ``user_details`` table like so:

.. code-block:: http
   :emphasize-lines: 10

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <user-auth-token>

   {
       "type":"insert",
       "args":{
           "table":"user_details",
           "objects":[
               {"user_id":"<user-id>", "access_token": "<facebook-access-token>"},
           ]
       }
   }

Now, whenever you want the user`s facebook ``access_token``, you can make a select query to fetch the data:

.. code-block:: http
   :emphasize-lines: 9

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <user-auth-token>

   {
       "type":"select",
       "args":{
           "table":"user_details",
           "columns": ["access_token"]
       }
   }

You can extend this pattern to storing any custom information you want about the user that you get from the social login or want to store otherwise, like their profile picture, likes, dislikes, friends list etc.
