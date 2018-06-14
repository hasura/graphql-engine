.. _data-permissions:

Data permissions and access control
===================================

Access to data stored in Hasura database is managed using :doc:`user roles <../auth/authorization/index>`.
By default, the data APIs can only be accessed by users with the ``admin`` role. However, we need to allow access to
the data APIs for roles other than ``admin``. This is handled by the permission layer of the ``Data`` microservice,
which lets you define **row level** and **column level** access control policies for various roles and query types.

What are the typical roles other than ``admin``?

#. ``user`` for logged in users
#. ``anonymous`` for users who haven't logged in.

We need to define permissions on all the tables that we have created for all available roles as needed.

How it works?
-------------

The Hasura :doc:`API gateway <../gateway/index>` acts as session middleware and forwards ``X-Hasura-*`` headers with
each request to the ``Data`` microservice. So, when an API call is made with an ``auth_token`` representing some user, a
variable called ``X-Hasura-User-Id`` is updated with the ``hasura_id`` of the user and a variable called ``X-Hasura-Role``
is updated with the role of the user making the call. These variables can now be used to describe the access permissions
for rows in tables. If the ``X-Hasura-Role`` header is passed with the request, its value is passed to the ``Data``
microservice if the user has that particular role or else the request is rejected with a ``403 Forbidden`` response.

Setting permissions
-------------------

We can use data APIs or the API console to set permissions on tables.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API console

      Say we have a table called ``article``.

      In the :doc:`API console <../api-console/index>`, navigate to *Data -> article -> Permissions*.

      This is the permissions section for the ``article`` table, which looks like this:

      .. image:: ../../img/complete-tutorial/tutorial-9-vanilla-screen.png

      To add permissions, click the *Edit icon* for the corresponding role and query type:

      .. image:: ../../img/complete-tutorial/tutorial-9-add-permission.png

      Here you can add permissions for the Select, Insert, Update, Delete query types for all available roles.

   .. tab:: Data APIs

      Refer :doc:`this <../api-reference/data/query/permission>` for the API reference for permissions data APIs
