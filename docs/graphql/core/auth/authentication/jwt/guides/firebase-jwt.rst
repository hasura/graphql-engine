.. meta::
   :description: Firebase JWT authentication for Hasura
   :keywords: hasura, docs, guide, authentication, auth, jwt, integration, firebase

.. _guides_firebase_jwt:

Firebase JWT integration
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will show how to integrate Firebase JWT authentication with Hasura.

.. _set_up_firebase_project:

Step 1: Set up a Firebase project
---------------------------------

Log into the `Firebase console <https://console.firebase.google.com/>`__.

Step 1.1: Create a project
^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a new project on Firebase by clicking on ``Add project``.

.. thumbnail:: /img/graphql/core/auth/firebase-create-project.png
   :alt: Create project on Firebase
   :width: 700px

Choose a project name and click ``Continue``.

.. thumbnail:: /img/graphql/core/auth/firebase-project-name.png
   :alt: Choose a project name for Firebase
   :width: 700px

Click through the next two steps to enable / disable Google analytics and then click on ``Create project``.

.. thumbnail:: /img/graphql/core/auth/firebase-finish-create-project.png
   :alt: Create Firebase project
   :width: 1000px

Step 1.2: Enable Realtime Database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next, we have to activate the Firebase realtime database. We need it to store our users that sign up.
On the left-hand navigation, click on ``Realtime Database`` and then click on ``Create Database``:

.. thumbnail:: /img/graphql/core/auth/firebase-create-realtime-db.png
   :alt: Create realtime DB on Firebase
   :width: 1000px

As for the security rules, you can select either. ``Test mode`` is enough for this tutorial.

.. thumbnail:: /img/graphql/core/auth/firebase-security-rules.png
   :alt: Define security rules for Firebase
   :width: 700px

Then click ``Enable``.

Step 1.3: Choose sign-in methods
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now you can choose the sign-in methods you want to use. On the left-hand navigation, click on ``Authentiction`` and then click on ``Sign-in method``:

.. thumbnail:: /img/graphql/core/auth/firebase-signin-method.png
   :alt: Define security rules for Firebase
   :width: 1000px

Step 1.4: Define rules
^^^^^^^^^^^^^^^^^^^^^^

Finally, we need define conditions for when data can be read by users.

Click on the ``Rules`` tab and replace the existing rules with the rules you want to use.
The below example specifies that a user can read their data that has their ``uid``.

.. thumbnail:: /img/graphql/core/auth/firebase-db-rules.png
   :alt: Firebase realtime DB rules
   :width: 1000px

Then hit ``Publish``.

.. note::

   Read more about rules in the `Firebase documentation <https://firebase.google.com/docs/rules/rules-and-auth>`__.

.. _configure_jwt_mode:

Step 2: Configure JWT mode for Hasura
-------------------------------------

In your Hasura project, add the following environment variables:

- ``HASURA_GRAPHQL_ADMIN_SECRET``: ``<your-admin-secret>``

- ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE``: ``anonymous``

- ``HASURA_GRAPHQL_JWT_SECRET``: 

.. code-block:: json

    {
        "type":"RS256",
        "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com",
        "audience": "<firebase-project-id>",
        "issuer": "https://securetoken.google.com/<firebase-project-id>"
    }

You can find your Firebase project ID by clicking on the gear icon on the Firebase console:

.. thumbnail:: /img/graphql/core/auth/firebase-settings.png
   :alt: Firebase settings
   :width: 1000px

Then you find the Firebase project ID under ``Your project``.

.. thumbnail:: /img/graphql/core/auth/firebase-project-id.png
   :alt: Firebase project id
   :width: 1000px

Click on ``Create project``.

.. thumbnail:: /img/graphql/core/auth/firebase-finish-create-project.png
   :alt: Firebase project id
   :width: 1000px

Step 3: Configure custom claims
-------------------------------

As per the :ref:`Hasura JWT spec <auth_jwt_spec>`, Hasura needs custom claims to be sent alongside the JWT token. 
We'll configure these custom claims using `Google Cloud functions <https://firebase.google.com/docs/functions/get-started>`__.

After installing the `Firebase CLI <https://firebase.google.com/docs/cli>`__, log in:

.. code-block:: bash

    firebase login

Then initialize the function in your terminal:

.. code-block:: bash

    firebase init functions

Choose the option ``Use an existing project`` and choose the project that we created in :ref:`step 1 <set_up_firebase_project>`.

.. thumbnail:: /img/graphql/core/auth/firebase-init-function.png
   :alt: Firebase init function
   :width: 700px

Then go through the following steps:

- Choose the language. For this tutorial, we'll use TypeScript.
- Choose if you want to use ``TSLint``. It's up to you.
- Choose to install dependencies.

Now add the example code below to ``functions/src/index.ts``. The example assumes that the default role is ``user`` and the allowed roles in this case only include ``user``.
All other requests fall in the role ``anonymous``, as defined in :ref:`step 2 <configure_jwt_mode>`.

.. code-block:: javascript

    import * as functions from 'firebase-functions'
    import * as admin from 'firebase-admin'

    admin.initializeApp(functions.config().firebase);

    export const processSignUp = functions.auth.user().onCreate(async (user) => {
      const customClaims = {
        "https://hasura.io/jwt/claims": {
        "x-hasura-default-role": "user",
        "x-hasura-allowed-roles": ["user"],
        "x-hasura-user-id": user.uid
        }
      };

      try {
        await admin
          .auth()
          .setCustomUserClaims(user.uid, customClaims);
          const metadataRef = admin.database().ref("metadata/" + user.uid);
          return metadataRef.set({ refreshTime: new Date().getTime() });
        } catch (error) {
            console.log(error);
        }
    });


Then deploy the function by running:

.. code-block:: bash

    firebase deploy --only functions


Step 4: Test use cases
----------------------

Let's assume you have an app that enables users to manage their todos. We want logged in users to be able to insert, select, update and delete their own todos.
Users who are not logged in can't see or manage any todos.

Add the following table to your database:

.. code-block:: sql

  todos (
    id SERIAL PRIMARY KEY,
    title TEXT,
    description TEXT,
    user_id INT
  )

Insert some sample data, so that we can later query the table.

Anonymous (not logged in)
^^^^^^^^^^^^^^^^^^^^^^^^^

With the anonymous role, you shouldn't be able to see anything regarding the management of todos.

User (logged in)
^^^^^^^^^^^^^^^^

- On the ``todos`` table, add a role ``user`` and give it insert, select, update and delete :ref:`permissions <authorization>`.
- Make sure you add the following custom check in all permissions: ``{"user_id":{"_eq":"X-Hasura-User-Id"}}``.
- Log in.
- You should be able to manage todos.


Next steps
----------

- Add more `Firebase rules <https://firebase.google.com/docs/rules>`__.
- Add more :ref:`roles <roles_variables>` depending on your use case.
- Apply :ref:`permissions <permission_rules>` to other tables in your project.
