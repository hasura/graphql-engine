.. .. meta::
   :description: Manual for adding a custom authentication provider to Hasura Auth
   :keywords: hasura, docs, auth, authentication, custom, provider


Custom provider based authentication
====================================

Hasura Auth provides APIs to signup and login users. You can integrate your own
authentication to work with Hasura Auth by configuring a custom provider in the
auth config. Users can authenticate with your custom provider similar to the
way they authenticate with default providers i.e. by giving the provider name
along with any custom data in the POST body of auth APIs.

An example for signing up with custom provider:

.. code-block:: http

  POST http://auth.test42.hasura-app.io/v1/signup HTTP/1.1
  Content-Type: application/json
  
  {
    "provider": "myCustomProvider",
    "data": {
      "customId": "myUser",
      "password": "pass123"
    }
  }


Prerequisites
-------------

Your custom provider should be a http(s) microservice which provides the following APIs:

* **signup**: For signing-up a new user
* **login**: For logging-in user
* **merge**: For merging user with a pre-existing Hasura user
* **createUser**: For creating user as admin
* **deleteUser**: For deleting user as admin

The functionality and interface of these APIs is described in subsequent sections.

Configuring custom provider
---------------------------

To enable a custom provider, you need to add the following configuration in
``conf/auth.yaml`` in your hasura project:

* Under ``customProviders``

.. code-block:: yaml

  customProviders:
    providerName:
      enabled: Bool
      defaultRoles: [Roles]
      hooks:
        signup: String
        login: String
        merge: String
        createUser: String
        deleteUser: String

* **providerName**: A name for the provider
* **enabled**: Whether this provider should be enabled
* **defaultRoles**: Default roles to be provided to user upon sign-up. For e.g. ["admin", "user"]
* **signup**: Signup hook of your provider. For e.g. http://myprovider.hasura-app.io/signup
* **login**: Login hook of your provider. For e.g. http://myprovider.hasura-app.io/login
* **merge**: Merge hook of your provider. For e.g. http://myprovider.hasura-app.io/merge
* **createUser**: Create user hook of your provider. For e.g. http://myprovider.hasura-app.io/create-user
* **deleteUser**: Delete user hook of your provider. For e.g. http://myprovider.hasura-app.io/delete-user

Below, we will describe the functionality and interface for each of the APIs in detail.


Signup
------

This API should be used to register a new user with your provider. This API
will receive the following JSON data from Hasura Auth:

.. code-block:: json

    {
      "hasura_id": "Int",
      "data": "Object"
    }

* **hasura_id**: A unique id used to identify user in Hasura Auth

* **data**: A JSON object which is specific to your provider (same as the
  ``data`` object passed to Hasura Auth during signup)

Hasura Auth expects the following response upon successful signup:

**Response**:

.. sourcecode:: http

  HTTP/1.1 200 OK
  Content-Type: application/json

  {
    "hasura_id": "Int",
    "create_session": "Bool",
    "merge_data": {"email": "String"},
    "new_user": "Bool"
  }

* **hasura_id**: Same as received in request.

* **create_session**: Boolean flag to indicate if a session should be created
  after signing up

* **merge_data**: A JSON object with one or more of these three fields:
  "username", "email", "mobile". This is used to merge accounts if any of the
  fields matches with a user already existing in Hasura Auth.

* **new_user**: Boolean flag indicating whether this is a new user. For most
  cases, this will be True


Login
-----

This API should be used to login a user with your provider. This API will
receive the following JSON data from Hasura Auth:

.. code-block:: json

    {
      "data": "Object"
    }

* **data**: A JSON object which is specific to your provider (same as the
  ``data`` object passed to Hasura Auth during signup)


**Response**:

.. sourcecode:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
        "hasura_id": "Int",
        "create_session": "Bool"
      }

* **hasura_id**: The Hasura Id of the user to be logged in

* **create_session**: Boolean flag to indicate if a session should be created
  after logging in. For most cases, this will be True


Merge
-----

This API should be used to merge an existing user with another user. A merge
may be required when a new user signs up with an identifier field same as an
existing user. Identifier fields supported by Hasura Auth are: username, email,
password. This API will receive JSON data from Hasura Auth:

.. code-block:: json

  {
    "old_hasura_id": "Int",
    "new_hasura_id": "Int"
  }

* **old_hasura_id**: The Hasura Id of the user which needs to be updated

* **new_hasura_id**: The Hasura Id with which to update the above user

**Response**:

.. sourcecode:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
        "success": "Bool"
      }

* **success**: Boolean flag indicating whether the merge was successful

Create User
-----------

This API should be used to create a new user with your provider. This API
will receive the following JSON data from Hasura Auth:

.. code-block:: json

    {
      "hasura_id": "Int",
      "data": "Object"
    }

* **hasura_id**: A unique id used to identify user in Hasura Auth

* **data**: A JSON object which is specific to your provider (same as the
  ``data`` object passed to Hasura Auth during create-user request)

Hasura Auth expects the following response upon successful creating user:

**Response**:

.. sourcecode:: http

  HTTP/1.1 200 OK
  Content-Type: application/json

  {
    "hasura_id": "Int",
    "user_data": {"email": "String"},
    "extra_info": "Object"
  }

* **hasura_id**: Same as received in request.

* **user_data**: A JSON object with one or more of these three fields:
  "username", "email", "mobile". This is used to merge accounts if any of the
  fields matches with a user already existing in Hasura Auth.

* **extra_info**: A JSON object which contains extra information about user created ( It is sent
  back to client as ``extra_info`` object in response)

Delete User
-----------

This API should be used to delete a user with your provider. This API
will receive the following JSON data from Hasura Auth:

.. code-block:: json

    {
      "hasura_id": "Int"
    }

* **hasura_id**: A unique id used to identify user in Hasura Auth

Hasura Auth expects the following response upon successful delete:

**Note**: Do not throw any HTTP errors if user not found or delete user was unsuccessful. Instead convey the information in response through boolean flags specified below.


**Response**:

.. sourcecode:: http

  HTTP/1.1 200 OK
  Content-Type: application/json

  {
    "user_exists": "Bool",
    "user_deleted": "Bool"
  }

* **user_exists**: Boolean flag indicating whether the user exists or not. If user does not exist, set ``user_deleted`` to ``false``.
* **user_deleted**: Boolean flag indicating whether the delete user was successful.



Errors
------

All errors should return a JSON object of the following form:

.. code-block:: json

  {
   "code": "String",
   "message": "String"
  }

* **code**: A short code indicating the class of error
* **message**: A detailed message about the error

