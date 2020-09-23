.. meta::
   :description: Custom webhook integration with NodeJS for Hasura
   :keywords: hasura, docs, guide, authentication, auth, webhook, integration, nodejs

.. _guides_custom_webhook:

Custom webhook server with NodeJS
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will show how to write a custom webhook server for Hasura using Node & Express JS.

Before you begin
----------------

Before starting with this guide, you should have the following ready:

- An empty Node & ExpressJS app

- An account on Heroku

Step 1: Set up tables in Hasura
-------------------------------

First, :ref:`create the following table on Hasura <create-tables>`:

.. code-block:: sql

   user (id int SERIAL, email text UNIQUE NOT NULL, password text NOT NULL, auth_token text NOT NULL, role text DEFAULT 'user' NOT NULL )

Step 2: Set up auth webhook
---------------------------

Inside your main file (you can call it ``app.ts`` or ``server.ts``), add the following code:

.. code-block:: javascript

  import 'cross-fetch/polyfill';
  import * as express from 'express';
  import * as bcrypt from 'bcryptjs';
  import * as crypt from 'crypto';

  const app = express()
  const port = parseInt(process.env.PORT, 10) || 3000

  // middleware
  app.use(express.json());
  app.use(express.urlencoded());

  /**
  * Creates a random token for authenticating a user.
  * Stored in "user.auth_token" in DB table and used to look current user up.
  */
  const createToken = () => crypt.randomBytes(16).toString("hex");

  /**
  * Fetches a user from database via Hasura
  * @param whereClause The boolean expression object to filter users by
  */
  async function findUserOne(whereClause) {
    const request = await fetch(
      "'http://localhost:8080/v1/graphql'",
      {
        method: "POST",
        headers: {
          "X-Hasura-Admin-Secret": process.env.HASURA_GRAPHQL_ADMIN_SECRET,
        },
        body: JSON.stringify({
          query: `
          query FindUser($where: user_bool_exp!) {
            user(where: $where) {
              id
              email
              password
              auth_token
              role
            }
          }
        `,
          variables: { where: whereClause },
        }),
      }
    );
    const firstResult = await request.json()
    return firstResult.data.user[0]
  }

  /**
  * Creates a user in database via Hasura, returns user record
  * @param user The user object to insert
  */
  async function createUser(user) {
    const request = await fetch("http://localhost:8080/v1/graphql", {
      method: "POST",
      headers: {
        "X-Hasura-Admin-Secret": "process.env.HASURA_GRAPHQL_ADMIN_SECRET",
      },
      body: JSON.stringify({
        query: `
          mutation InsertUser($user: user_insert_input!) {
            insert_user_one(object: $user) {
              id
              auth_token
            }
          }
        `,
        variables: { user },
      }),
    });
    return request.json()
  }

  /**
  * Create a new user record and generate Auth Token in DB
  */
  app.post('/signup', async (req, res) => {
    const user = req.body.user
    // Securely hash password and generate random auth token
    user.password = await bcrypt.hash(user.password, 10)
    user.auth_token = await createToken()
    const insertUserResponse = await createUser(user)
    return res.json(insertUserResponse)
  })

  /**
  * Sign user in by looking up email and comparing password, return user record with auth token from DB
  */
  app.post('/login', async (req, res) => {
    const user = req.body.user
    const userRecord = await findUserOne({ email: { _eq: user.email } })
    const validPassword = await bcrypt.compare(user.password, userRecord.password)
    if (!validPassword) return res.status(400).json({ error: 'Invalid credentials' })
    return res.json(userRecord)
  })

  /**
  * Authentication webhook for Hasura
  * Looks a user up in DB using "Bearer <auth token>" value from "Authorization" header
  */
  app.get('/webhook', async (req, res) => {
    // Extract token from request
    const authHeader = req.get('Authorization')
    const [scheme, token] = authHeader.split(' ')

    if (scheme != 'Bearer')
      return res.status(400).json({ error: `Invalid Auth scheme, expected type "Bearer"` })

    const user = await findUserOne({ auth_token: { _eq: token } })
    const hasuraSessionVariables = {
      "X-Hasura-Role": user.role,
      "X-Hasura-User-Id": `${user.id}`,
    };
    return res.json(hasuraSessionVariables)
  })

  // Start server on PORT, bind to 0.0.0.0 host for Docker support in addition to localhost
  app.listen(port, '0.0.0.0', () => {
    console.log('Your app is listening on port ' + port);
  })

Step 3: Deploy the webhook
--------------------------

Now let's deploy the webhook. You can deploy it to any cloud provider. In this example, we'll use Heroku.

Step 3.1: Commit your webhook to Git
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inside your project directory, run:

.. code-block:: bash

  git init && git add . && git commit -m "init auth webhook"

Step 3.2: Set up a Heroku app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inside your project directory, run:

.. code-block:: bash

  heroku apps:create
  git push heroku master

You will get back a URL with your deployed webhook. If you open it in your browser, you will see the following: ``Webhooks are running.``

Step 3: Test webhook endpoints
------------------------------

Let's test the endpoint we created in step 2. 

Step 3.1: Signup
^^^^^^^^^^^^^^^^

Send a request to the ``signup`` endpoint (e.g. ``https://your-app-url.herokuapp.com/signup``) with the following request body:

.. code-block:: json

  {
    "user": {
      "email": "myemail@email.com",
      "password": "password",
      "role": "user"
    }
  }

You should get back a response that looks like this:

.. code-block:: json

  {
      "data": {
          "insert_user_one": {
              "id": 7,
              "auth_token": "1aeb90035b2e09cd61637a38b0fda25e"
          }
      }
  }

Step 3.1: Login
^^^^^^^^^^^^^^^

Send a request to the ``signup`` endpoint (e.g. ``https://your-app-url.herokuapp.com/login``) with the following request body:

.. code-block:: json

  {
    "user": {
      "email": "myemail@email.com",
      "password": "password",
      "role": "user"
    }
  }

You should get back a response that looks like this:

.. code-block:: json

  {
    "id": 7,
    "email": "myemail@email.com",
    "password": "$2a$10$X5pWGkNEEwN8R//OMTra8uBsLfpoTWrlnLDfRr9HJg918WCYN.j.m",
    "auth_token": "1aeb90035b2e79cd60637a38b0fda25e",
    "role": "user"
  }

Step 5: Set up webhook mode in Hasura
-------------------------------------

There are two options to configure Hasura to run in webhook mode:

1. Running the GraphQL engine with the ``--auth-hook`` flag 
2. Add the ``HASURA_GRAPHQL_AUTH_HOOK`` environment variable 

The value is the webhook endpoint. In this tutorial, the endpoint looks like this: ``https://my-auth-webhook.herokuapp.com/webhook`` where ``my-auth-webhook`` should be replaced by your own app name.

.. note::

  See :ref:`GraphQL engine server options <server_flag_reference>` for more information on flags and environment variables.

Step 6: Test your auth webhook
------------------------------

Make a 

From now on, whenever a request comes in to Hasura, the auth webhook will be called. 

Make an API call to your Hasura endpoint and see how the webhook returns the ``role`` and the ``user_id``.


