.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Android Integration Guide
=====

Follow this guide to get a basic understanding of how to get started with Hasura on Android. We will be using `Retrofit <https://square.github.io/retrofit/>`_  and `Gson <https://github.com/google/gson/>`_  to make the API calls in our Android app. If you are unfamiliar with Hasura, it is recommended that you go through the `Getting Started Guide <https://hasura.io/_docs/platform/0.6/getting-started/>`_.

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
   console, this will be ``email`` and similarly, ``mobile``. If you have not
   enabled either, then this will be ``username``.


.. code-block:: http

   POST auth.<project-name>.hasura-app.io/login HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456"
   }

Using Gson, the above request can be translated to the following in Java:

.. literalinclude:: LoginRequest.java
   :language: java 

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

Converting the response to Java using Gson would look like : 

.. literalinclude:: LoginResponse.java
   :language: java

Setting up Retrofit to consume these apis includes the following steps : 

* ``AuthInterface`` is a Java Interface for the APIs, in this case the Login API

.. literalinclude:: AuthInterface.java
   :language: java
  
* Building the Retrofit client

.. code-block:: java 
   
   final Retrofit retrofit = new Retrofit.Builder()
                .baseUrl("https://auth.<project-name>.hasura-app.io")
                .addConverterFactory(GsonConverterFactory.create())
                .build();
   AuthInterface auth = retrofit.create(HasuraAuthInterface.class);   

The implementation would look like the following :

.. code-block:: java 
   
   auth.login(new LoginRequest("username","password")).enqueue(new Callback<LoginResponse>() {
            @Override
            public void onResponse(Call<LoginResponse> call, Response<LoginResponse> response) {
                if (response.isSuccessful()) {
                  //Handle successful response
                  LoginResponse loginResponse = response.body();
                } else {
                  //Handle Error
                }
            }

            @Override
            public void onFailure(Call<LoginResponse> call, Throwable t) {
            }
        });

Similarly, integrating the LOGOUT API would look like :

.. code-block:: http
   :emphasize-lines: 2

   GET auth.<project-name>.hasura-app.io/user/logout HTTP/1.1
   Authorization: Bearer pand3yofqfqrz7kab8i7n4w9n2wgc6bg

.. code-block:: java 
      
   @GET("user/logout")
   Call<MessageResponse> logout(@Header("Authorization") String authHeader);

Note that the authHeader being sent above needs to be of the form ``Bearer <auth_token>``

The response of the Logout API would be the following :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
        "message": "Logged out"
   }


Using a class named MessageResponse to handle the above response using Gson :

.. literalinclude:: MessageResponse.java
   :language: java

And implementing the API would look like :

.. code-block:: java 
   
   String authHeader = "Bearer " + savedAuthToken;
   auth.logout(authHeader).enqueue(new Callback<MessageResponse>() {
            @Override
            public void onResponse(Call<MessageResponse> call, Response<MessageResponse> response) {
                if (response.isSuccessful()) {
                  //Clear all sessions for this particular user
                } else {
                  //Handle Error
                }
            }

            @Override
            public void onFailure(Call<LoginResponse> call, Throwable t) {
            }
        });

When the user logs out, the auth token associated with that user is invalidated by Hasura. Henceforth, the user needs to be re-autheticated to make any API calls requiring authorization.

Managing Sessions
^^^^^^^^^^^^^^^^^

You need to have your own mechanism of storing the authentication tokens (``auth_token``) and managing them. That means, storing and updating them whenever a Hasura Auth API returns a new authentication token, and remove all the existing authentication tokens. Click `here <https://gist.github.com/jaisontj/d9c9b2382d8143007ebc2f06253329e4#file-hasura-todo-android-initial-java>`_ for an example of a class that handles authentication tokens.

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
 
Representing this table in Java :

.. literalinclude:: ArticleRecord.java
   :language: java

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

In Java, this would be :

.. literalinclude:: InsertQuery.java
   :language: java

The response would be :

.. code-block:: http 
   
   {
	"affected_rows":2,
	"returning": [
		{"id":1,"name":"News"},
		{"id":2,"name":"Movies"}
	]
   }

The Java class to handle this response would be :

.. literalinclude:: ReturningResponse.java
   :language: java

Our API call definition would be 

.. code-block:: java 
      
   @POST("query")
   Call<ReturningResponse> insertArticles(@Header("Authorization") String authHeader, @Body InsertQuery body);


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

In Java

.. literalinclude:: SelectQuery.java
   :language: java

This query returns an Array of ``ArticleRecord``

The API definition would be :

.. code-block:: java 

   @POST("query")
   Call<List<ArticleRecord>> getArticles(@Header("Authorization") String authHeader, @Body SelectQuery body);


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

In Java, 

.. literalinclude:: UpdateQuery.java
   :language: java

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

.. literalinclude:: DeleteQuery.java
   :language: java






.. toctree::
  :maxdepth: 2

