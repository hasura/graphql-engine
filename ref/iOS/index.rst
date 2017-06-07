.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

iOS Integration Guide
=====================

Follow this guide to get a basic understanding of how to get started with Hasura on iOS using Swift. We will be using `Retrofit <https://github.com/Alamofire/Alamofire>`_  and `ObjectMapper <https://github.com/Hearst-DD/ObjectMapper>`_  to make the API calls in our Android app. If you are unfamiliar with Hasura, it is recommended that you go through the `Getting Started Guide <https://hasura.io/_docs/platform/0.6/getting-started/>`_.

Hasura provides two major types of API endpoints ``AUTH`` and ``DATA`` 

Auth
----

The AUTH APIs are used to authenticate the users. These include LOGIN, REGISTRATION, LOGOUT among a few more.

The AUTH API endpoint is always of type:

.. code-block:: http

     https://auth.<project-name>.hasura-app.io

Let's have a look at integrating the LOGIN API

To log a user in, use the ``/login`` endpoint.

There are two mandatory parameters in the request body for a login action.

1. Password of the user.
2. The second will be - based on your configuration - username, email or
   mobile of the user. If you have enabled login via email in your project
   console, this will be ``email`` and similarly ``mobile``. If you have not
   enabled either of them, then this will be ``username``.


.. code-block:: http

   POST auth.<project-name>.hasura-app.io/login HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456"
   }

Using ObjectMapper, the above request can be translated to the following in Swift:

.. literalinclude:: LoginRequest.swift
   :language: swift

Typical response of the ``/login`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "tpdq0m9whrj7i4vcjn48zq43bqx2",
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }

* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

Converting the response to Swift using ObjectMapper would look like : 

.. literalinclude:: LoginResponse.swift
   :language: swift

Before we start using ``Alamofire`` to consume these APIs, we need to ensure that Alamofire and ObjectMapper work together in parsing the response from the APIs. Check out `AlamofireObjectMapper <https://github.com/tristanhimmelman/AlamofireObjectMapper>`_ to do this.

Using Alamofire to make the LOGIN API call would look like :

.. code-block:: swift

   let headers: HTTPHeaders = [
	"Content-type" : "application/json"
   ]
   let params = AuthRequest(username: "username",password: "password").toJSON()
   Alamofire.request("https://auth.<project-name>.hasura-app.io/login",method: .post, parameters: params, encoding: JSONEncoding.default).responseObject { (response: DataResponse<AuthResponse>) in
            switch response.result {
            case .success(let authResponse):
                //Handle Success
                break
            case .failure(let error)
                //Handle Error
                break
            }
        }

Similarly, integrating the LOGOUT API would look like :

.. code-block:: http
   :emphasize-lines: 2

   GET auth.<project-name>.hasura-app.io/user/logout HTTP/1.1
   Authorization: Bearer pand3yofqfqrz7kab8i7n4w9n2wgc6bg

.. code-block:: swift
 
   let headers: HTTPHeaders = [
        "Content-type" : "application/json",
        "Authorization": "Bearer <auth_token>"
   ]
   Alamofire.request("https://auth.<project-name>.hasura-app.io/user/logout",method: .get, encoding: JSONEncoding.default).responseObject { (response: DataResponse<MessageResponse>) in
            switch response.result {
            case .success(let messageResponse):
                //Handle Success
                //Clear Sessions
                break
            case .failure(let error)
                //Handle Error
                break
            }
        }
Note that the authHeader being sent above needs to be of the form ``Bearer <auth_token>``

The response of the Logout API would be the following :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
        "message": "Logged out"
   }


Using a class named MessageResponse to handle the above response using Swift :

.. literalinclude:: MessageResponse.swift
   :language: swift

   
When the user logs out, the auth token associated with that user is invalidated by Hasura. Henceforth, the user needs to be re-autheticated to make any API calls requiring authorization.

Managing Sessions
^^^^^^^^^^^^^^^^^

You need to have your own mechanism of storing the authentication tokens (``auth_token``) and managing them. That means, storing and updating them whenever a Hasura Auth API returns a new authentication token, and remove all the existing authentication tokens. Click `here <https://gist.github.com/jaisontj/2b180b4a04724787efc94a0bcd469f46#file-hasura-swift>`_ for an example of a class that handles authentication tokens.

Data
----

The data service on Hasura exposes an HTTP/JSON API over a PostgreSQL database.
This API is designed to be used by any web client (speaking JSON), especially
frontend interfaces like Android and iOS apps, browser based JavaScript apps
etc.

The DATA endpoint is always of type :

.. code-block:: http

     https://data.<project-name>.hasura-app.io

Data service unifies all operations that can be performed on the database under
a single query interface.

Data service exposes a query endpoint at ``v1/query``. A typical query
operation is as follows:

.. code-block:: http

     POST /v1/query HTTP/1.1


For posterity, consider a table named ``article`` which has three columns ``user_id``, ``name`` and ``id`` where ``id`` is the primary key and is an auto incremented value given to each row when it is inserted into the table, ``name`` is the name of the article and ``user_id`` is the id of the user who has written this article.
 
Representing this table in Swift :

.. literalinclude:: ArticleRecord.swift
   :language: swift

Let's have a look at the common use cases :

Inserting Data
^^^^^^^^^^^^^^

Let's insert a couple of categories. The full definition of `insert` request can be found :ref:`here <data_insert>`.

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth_token>

   {
       "type":"insert",
       "args":{
           "table":"article",
           "objects":[
               {"name":"News","user_id":<hasura_id>},
               {"name":"Movies","user_id":<hasura_id>}
           ],
           "returning":["id","name"]
       }
   }

In the code above, we are inserting two rows (``News`` and ``Movies``) into the table ``category``. 
Note the ``returning`` key. We would like to get the auto incremented id for each inserted row.

In Swift, this would be :

.. code-block:: swift

   let params: [String: Any] = [
            "type" : "insert",
            "args" : [
                "table"     : "article",
                "objects"   : [
                    [
                        "user_id"  : <user_id>,
                        "name"     : "News",
                    ],
                    [
                        "user_id"  : <user_id>,
                        "name"     : "Movies"
                    ]
                ],
                "returning" : ["id","title","completed"]
            ]
        ]

The response would be :

.. code-block:: http 
   
   {
	"affected_rows":2,
	"returning": [
		{"id":1,"name":"News"},
		{"id":2,"name":"Movies"}
	]
   }

The Swift class to handle this response would be :

.. literalinclude:: ReturningResponse.swift
   :language: swift

Our API call definition would be 

.. code-block:: swift 

         Alamofire.request("https://data.<project-name>.hasura-app.io/query", method: .post, parameters: params, encoding: JSONEncoding.default).responseObject(completionHandler: <(DataResponse<ReturningResponse>) -> Void>)

Selecting Data
^^^^^^^^^^^^^^

The JSON based query language lets you make simple to complex queries.

Let's look at a simple `select` query on the category table. The full definition of a `select` query can be found :ref:`here <data_select>`

.. code-block:: http

   POST data.<project-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth_token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": ["id", "name"],
           "where": {"user_id": 1}
       }
   }

This query returns ``id`` and ``name`` of rows from ``article`` table where ``user_id`` is ``1``.

In Swift

.. code-block:: swift 

   let params: [String: Any] = [
            "type" : "select",
            "args" : [
                "table"     : "article",
                "columns"   : ["id","name"],
                "where"     : [
                    "user_id" : 1
                ]
            ]
        ]

This query returns an Array of ``ArticleRecord``

The API definition would be :

.. code-block:: swift 

      Alamofire.request("https://data.<project-name>.hasura-app.io/query", method: .post, parameters: params, encoding: JSONEncoding.default).responseArray(completionHandler: <(DataResponse<[ArticleRecord]>) -> Void>)

Updating Data
^^^^^^^^^^^^^

The request to update data consists of two parts - the new values and a ``where`` indicating what to update. The syntax of where clause is same as in the `select` query. For the full syntax of update request, see :ref:`here <data_update>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "update",
       "args" : {
           "table" : "article",
           "$set": {"name": "Mysterious affair at Styles"},
           "where": {
               "id": 4,
	       "user_id": 1	 
           }
       }
   }

This query sets the ``name`` of rows from ``article`` table where ``user_id`` is ``1`` and ``id`` is ``4`` to ``"Mysterious affair at Styles"``

In Swift, 

.. code-block:: swift 

   let params: [String: Any] = [
            "type" : "update",
            "args" : [
                "table"     : "article",
                "$set"      : [
                    "name"    : "Mysterious affair at Styles",
                ],
                "where"     : [
                    "user_id" : 1,
                    "id"      : 4
                ]
            ]
        ]

The response will be of type, ``ReturningResponse``. Since, there is no ``returning`` key specified. The response will only contain value for the ``affected_rows``.

Deleting Data
^^^^^^^^^^^^^

The request to delete data takes a ``where`` clause indicating what to delete. The syntax of where clause is same as in the `select` query. For the full syntax of delete request, see :ref:`here <data_delete>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "delete",
       "args" : {
           "table" : "article",
           "where": {
              "id" : 1,
              "user_id": 1
           }
       }
   }

This query deletes the rows with ``id`` = ``1`` and ``user_id`` = ``1`` in the ``article`` table. The response will be of type ``ReturningResponse`` without any ``returning`` value since there is no ``returning`` key specified in the query.

.. code-block:: swift

   let params: [String: Any] = [
            "type" : "delete",
            "args" : [
                "table"     : "article",
                "where"     : [
                    "user_id" : 1,
                    "id"      : 4
                ]
            ]
        ]



.. toctree::
  :maxdepth: 2

