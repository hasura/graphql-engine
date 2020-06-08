.. meta::
   :description: Codegen for Hasura actions in Python & Flask
   :keywords: hasura, docs, actions, codegen, guide, python, flask

.. _actions_codegen_python:

GraphQL API with Python & Flask
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

In this example, we will discuss how to utilize Actions and Codegen to build a Python Flask API for JWT Authentication.

Our Action types will be two methods, ``Signup`` and ``Login``, that return a ``CreateUserOutput`` and ``JsonWebToken``. For this example, we are assuming a ``User`` table with fields ``email`` and ``password``:

.. code-block:: graphql

  type Mutation {
    Login (email: String! password: String!): JsonWebToken
  }

  type Mutation {
    Signup (email: String! password: String!): CreateUserOutput
  }

  type CreateUserOutput {
    id : Int!
    email : String!
    password : String!
  }

  type JsonWebToken {
    token : String!
  }

Create Action Definitions
--------------------------

Start by creating the Signup Action type definitions in the Console:

.. image:: python-flask-signup-types.png
  :width: 900px
  :alt: python-flask-signup-types
  :class: no-shadow

If we check the "Codegen" tab, we can see that a nice scaffold has been generated for us from these GraphQL types:


.. image:: python-flask-signup-codegen-1.png  
  :scale: 100%
  :alt: python-flask-signup-codegen1

.. image:: python-flask-signup-codegen-types.png
  :scale: 100%
  :alt: python-flask-signup-codegen-types

You may notice a couple of things about the generated code:

* The generated handler function returns a Dataclass instance of the auto-generated type matching it's expected return type
* There is a ``RequestMixin`` Dataclass which provides some utility functions that all other classes inherit from

Let's walk through the boilerplate and see what's happening.

.. code-block:: python

  from SignupTypes import SignupArgs, CreateUserOutput
  from flask import Flask, request, jsonify

  app = Flask(__name__)

  @app.route('/Signup', methods=['POST'])
  def SignupHandler():
    args = SignupArgs.from_request(request.get_json())
    print(args)
    # business logic here
    return CreateUserOutput().to_json()

  if __name__ == '__main__':
    app.run(debug = True, host = '0.0.0.0')

Here we have the base handler method for the Action generated. The route name will match the name of your Action, and so will the function name.
Our codegen module generates new GraphQL Types for the input arguments to all Action queries/mutations, and from that we generate language-specific types.

This is where ``SignupArgs`` comes from -- It's a Python Dataclass that matches the shape of your mutation argument input. With it, on the ``RequestMixin``
we provide a method ``.from_request()`` which can take a JSON object from HTTP request body and convert it to an instance of the class. So we invoke that
generic method and convert it for convenience.

Finally, we know from the schema that the return type needs to be a ```CreateUserOutput```, so we leave an empty instance there and call
the ``.to_json()`` helper method we define on all ``RequestMixin`` objects.


.. code-block:: python

  from dataclasses import dataclass, asdict
  from typing import List, Optional
  from enum import Enum, auto
  import json

  @dataclass
  class RequestMixin:
      @classmethod
      def from_request(cls, request):
          """
          Helper method to convert an HTTP request to Dataclass Instance
          """
          values = request.get("input")
          return cls(**values)

      def to_json(self):
          return json.dumps(asdict(self))

  @dataclass
  class CreateUserOutput(RequestMixin):
    id: int
    email: str
    password: str

  @dataclass
  class SignupArgs(RequestMixin):
    email: str
    password: str

Overview of Action Handler Implementation
-----------------------------------------

Now we need to implement the business logic for ``Signup``. Our Action will do the following:

* Recieve the Action ``email`` and ``password`` arguments on ``request``, and pass those values to ``SignupArgs.from_request()``
* Convert the plaintext password input into a hashed secure password with Argon2
* Send a mutation to Hasura to save the newly created user with the hashed password
* Return the created user object to signal success, or else error

The first thing we have to implement is the Argon2 password hashing. We will use `argon2-cffi <https://github.com/hynek/argon2-cffi>`_ for this. The second thing is a library for making requests to Hasura for mutations/queries, our choice will be `requests <https://github.com/psf/requests>`_.

Our ``requirements.txt`` will now look like: ::

  flask
  argon2-cffi
  requests

On to the implementation.

Signup Handler & Password Hashing
----------------------------------

For password hashing, the ``argon2`` API is minimal and straightforward: an instance of a password hasher is created with ``PasswordHasher()``, which has methods ``.hash(password)``, ``.verify(hashed_password, password)``, and `.check_needs_rehash(hashed_password)`.

In our signup handler, the first thing we'll do is convert the Action input password to a secure hash:

.. code-block:: python

  from argon2 import PasswordHasher
  Password = PasswordHasher()

  @app.route("/signup", methods=["POST"])
  def signup_handler():
      args = AuthArgs.from_request(request.get_json())
      hashed_password = Password.hash(args.password)

Creating the GraphQL Request Client
-----------------------------------

Next, since we have the user's email and hashed password, we need to send a request to Hasura to save them in the database. For that, we'll need a request client implementation:

.. code-block:: python

  from requests import request

  @dataclass
  class Client:
      url: str
      headers: dict

      def run_query(self, query: str, variables: dict, extract=False):
          request = requests.post(
              self.url,
              headers=self.headers,
              json={"query": query, "variables": variables},
          )
          assert request.ok, f"Failed with code {request.status_code}"
          return request.json()

      create_user = lambda self, email, password: self.run_query(
          """
              mutation CreateUser($email: String!, $password: String!) {
                  insert_user_one(object: {email: $email, password: $password}) {
                      id
                      email
                      password
                  }
              }
          """,
          {"email": email, "password": password},
      )

Here we create a utility class for handling our Hasura operations. It takes an URL and headers object as initialization options, and exposes a method ``.run_query()`` for performing GraphQL requests. We create the query function for saving our user in the Signup action as a class method as well.

We can instantiate the ``Client`` like this:

.. code-block:: python

  HASURA_URL = "http://graphql-engine:8080/v1/graphql"
  HASURA_HEADERS = {"X-Hasura-Admin-Secret": "your-secret"}

  client = Client(url=HASURA_URL, headers=HASURA_HEADERS)

Now, in our ``signup`` Action handler, we need to call ``client.create_user()`` with the input email and the hashed password value to save them, then return the result:

.. code-block:: python

  @app.route("/signup", methods=["POST"])
  def signup_handler():
      args = AuthArgs.from_request(request.get_json())
      hashed_password = Password.hash(args.password)
      user_response = client.create_user(args.email, hashed_password)
      if user_response.get("errors"):
          return {"message": user_response["errors"][0]["message"]}, 400
      else:
          user = user_response["data"]["insert_user_one"]
          return CreateUserOutput(**user).to_json()

To test this out, send an HTTP request to your Flask API at ``/signup`` with an email and password. You should get a successful response like this:

.. image:: python-flask-signup-request.png
  :width: 800px
  :alt: python-flask-signup-request
  :class: no-shadow

Now our Signup Action is functional! The last piece is create the Login handler, which will do a password comparison, and then return a signed JWT if successful.

Login Handler & JWT Token
--------------------------

The first thing we need a new request method on our ``Client`` class to find a user by email, so that we can look them up to compare password. Under ``create_user``, create a new method:

.. code-block:: python

  find_user_by_email = lambda self, email: self.run_query(
      """
          query UserByEmail($email: String!) {
              user(where: {email: {_eq: $email}}, limit: 1) {
                  id
                  email
                  password
              }
          }
      """,
      {"email": email},
  )

Then in our login handler, we call ``Password.verify()`` to compare the input password against the hashed password saved in the database. If the password matches, we create a JWT from the user credentials, and return it.

We also need to check to see if the password needs to be updated and re-hashed by Argon2, in the event that hashing parameters have changed and it's no longer valid. If so, we should re-hash and then save the updated password in the database through an update mutation to Hasura, ``client.update_password()``.

.. code-block:: python

  @app.route("/login", methods=["POST"])
  def login_handler():
      args = LoginArgs.from_request(request.get_json())
      user_response = client.find_user_by_email(args.email)
      user = user_response["data"]["user"][0]
      try:
          Password.verify(user.get("password"), args.password)
          rehash_and_save_password_if_needed(user, args.password)
          return JsonWebToken(generate_token(user)).to_json()
      except VerifyMismatchError:
          return { "message": "Invalid credentials" }, 401

Here is what the implementation of ``generate_token()``, ``rehash_and_save_password_if_needed()``, and look like:

.. code-block:: python

  import os
  import jwt

  # Try to get the secret from ENV, else fallback to provided string
  HASURA_JWT_SECRET = os.getenv("HASURA_GRAPHQL_JWT_SECRET", "a-very-secret-secret")

  # ROLE LOGIC FOR DEMO PURPOSES ONLY
  # NOT AT ALL SUITABLE FOR A REAL APP
  def generate_token(user) -> str:
      """
      Generates a JWT compliant with the Hasura spec, given a User object with field "id"
      """
      user_roles = ["user"]
      admin_roles = ["user", "admin"]
      is_admin = user["email"] == "admin@site.com"
      payload = {
          "https://hasura.io/jwt/claims": {
              "x-hasura-allowed-roles": admin_roles if is_admin else user_roles,
              "x-hasura-default-role": "admin" if is_admin else "user",
              "x-hasura-user-id": user["id"],
          }
      }
      token = jwt.encode(payload, HASURA_JWT_SECRET, "HS256")
      return token.decode("utf-8")

  def rehash_and_save_password_if_needed(user, plaintext_password):
      """
      Whenever your Argon2 parameters – or argon2-cffi’s defaults! – 
      change, you should rehash your passwords at the next opportunity.
      The common approach is to do that whenever a user logs in, since 
      that should be the only time when you have access to the cleartext password.
      Therefore it’s best practice to check – and if necessary rehash –
      passwords after each successful authentication.
      """
      if Password.check_needs_rehash(user["password"]):
          client.update_password(user["id"], Password.hash(plaintext_password))

And finally, ``client.update_password()``:

.. code-block:: python

  update_password = lambda self, id, password: self.run_query(
      """
          mutation UpdatePassword($id: Int!, $password: String!) {
              update_user_by_pk(pk_columns: {id: $id}, _set: {password: $password}) {
                  password
              }
          }
      """,
      {"id": id, "password": password},
  )

Testing out the Handler Routes
-------------------------------

Now, if we send a request to ``/login`` using our email and password from ``/signup``, we should successful get a signed JWT, and decoding the JWT should return the correct information:

.. image:: python-request-login.png  
  :scale: 90%
  :alt: python-request-login

.. image:: python-flask-jwt-decode.png
  :scale: 80%
  :alt: python-flask-jwt-decode


Calling the Finished Actions
-----------------------------

Now we can call our finished Action from Hasura's API, and validate our responses:

.. figure:: signup-mutation.png
  :width: 90 %
  :align: center

  Signup Mutation

.. figure:: signup-mutation-duplicate.png
  :width: 110 %
  :align: center

  Signup Mutation Error

.. figure:: login-query.png
  :width: 90 %
  :align: center

  Login Query

.. figure:: login-query-bad-password.png
  :width: 110 %
  :align: center

  Login Query Error

  