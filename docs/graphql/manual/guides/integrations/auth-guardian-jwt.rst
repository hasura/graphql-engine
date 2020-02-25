.. meta::
   :description: Integrate AuthGuardian JWT with Hasura
   :keywords: hasura, docs, guide, authentication, auth, jwt, integration

.. _auth_guardian_jwt:

AuthGuardian JWT Integration with Hasura GraphQL engine
=======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

In this guide, we will walk through how to set up AuthGuardian JWT to work with the Hasura GraphQL engine.

About AuthGuardian 
^^^^^^^^^^^^^^^^^^

`AuthGuardian <https://www.onegraph.com/docs/auth_guardian.html>`__
is a free service by `OneGraph <https://www.onegraph.com/>`__ that allows you to easily visually describe auth rules for your app, API or service.
It will generate JWTs that can be used for any service that supports JWTs.

AuthGuardian has built-in support for Hasura that works well with Hasura's permission system.

Step 1: Create an account
^^^^^^^^^^^^^^^^^^^^^^^^^

Create an account, an organization and a project on `OneGraph <https://www.onegraph.com/>`__.

Step 2: Add an auth rule
^^^^^^^^^^^^^^^^^^^^^^^^

On the left sidebar, chose "Auth Services" and then "AuthGuardian".

There are four options how to generate Hasura compatible JWTs:

Option 1: Set Hasura ``user_id``
--------------------------------

- For the section "When the user is on", select ``Always``.
- For the section "Then", choose ``On hasura set user id``.
- Add the user id in the "JSON" field, e.g. ``1``.
- Click the ``save`` button on the right hand side.

.. thumbnail:: ../../../../img/graphql/manual/guides/auth-guardian-user-id.png
   :alt: Set an auth guardian JWT with a user id

Option 2: Set the default Hasura role
-------------------------------------

- For the section "When the user is on", select ``Always``.
- For the section "Then", choose ``On hasura set default role``.
- Add the default role, e.g. ``user``.
- Click the ``save`` button on the right hand side.

.. thumbnail:: ../../../../img/graphql/manual/guides/auth-guardian-default-role.png
   :alt: Set an auth guardian JWT with default role

.. note::

   Note that a second claim ``x-hasura-allowed-roles`` was added and the default role was added to it.

Option 3: Allow additional Hasura roles
---------------------------------------

- For the section "When the user is on", select ``Always``.
- For the section "Then", choose ``On hasura add roles``.
- Click on "Add" and add an additional role, e.g. ``contributor``.
- Click the ``save`` button on the right hand side.

.. thumbnail:: ../../../../img/graphql/manual/guides/auth-guardian-additional-role.png
   :alt: Set an auth guardian JWT with additional roles

Option 4: Set a session variable
--------------------------------

- For the section "When the user is on", select ``Always``.
- For the section "Then", choose ``On hasura set session variable``.
- Add your session variable name and the value.
- Click the ``save`` button on the right hand side.

SCREENSHOT

Step 3: Generate the JWT config
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- On the left sidebar, click on "JWT Settings" and scroll down to "Configuration generator"
- Chose either "Hasura" or "Haura on Heroku"

The generated config has the following structure:

.. code-block:: json

        {
        "type": "RS256",
        "jwk_url": "https://serve.onegraph.com/app/35bcf98c-1df0-4644-a453-bf06a1349449/.well-known/jwks.json",
        "claims_format": "json"
        }

- Add the generated config as a value for the environment variable ``HASURA_GRAPHQL_JWT_SECRET`` or for the ``--jwt-secret`` server flag.

Step 4: Test the config
^^^^^^^^^^^^^^^^^^^^^^^

- Copy the JWT that you created in step 2.
- On the left sidebar, click on "JWT settings" and scroll down to "Generate signed token".
- Paste the copied JWT.
- Copy the signed token and add it as a header in the Hasura console.

.. thumbnail:: ../../../../img/graphql/manual/guides/auth-guardian-test-jwt.png
   :alt: Test AuthGuardian JWT

- On GraphiQL, try out queries to test that the integration works as expected.
