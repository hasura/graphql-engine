.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


.. .. meta::
   :description: Reference documentation for web SDKs used for integrating frontend code with backend APIs (both Hasura micro-services and custom microservices). 
   :keywords: hasura, docs, Javascript SDK, integration

Web SDKs
========
The following is a list of web SDKs that can be used to integrate frontend code with backend APIs. They can be used to integrate both Hasura micro-services (Data, Auth/User-management and FileStore) and custom microservices.

JavaScript SDK
--------------

This SDK helps you integrate your Web application with Hasura. It makes data queries and user management exceptionally easy.

Installation
~~~~~~~~~~~~

Installation is as simple as dropping a script tag as follows

.. code:: html 

    <body>
      <script src="https://github.com/hasura/js-sdk/releases/download/v0.1.3/hasura.min.js">
      </script>
      <script>
        hasura.setProject('hello70'); // If your hasura project is hello70.hasura-app.io
      </script>
    </body>

Quickstart
~~~~~~~~~~

.. code:: javascript

    /* New session */
    hasura.user // Will be anonymous user
    // {
    //     username: 'anonymous',
    //     id: 0,
    //     roles: ['anonymous'],
    //     token: null
    // }
    
    /* Login and create new session */
    hasura.setUsername('user1'); // Will set username for current object and save to localStorage
    hasura.auth.login('user1password', onSuccess, onError); // Will log the current user
    hasura.user // will be logged in user
    // {
    //     username: 'user1',
    //     id: 3,
    //     roles: ['user'],
    //     token: 'xxxxxxxxxxxxxxxx'
    // }
    
    /* If you refresh the page */
    hasura.user // will be the logged in user
    // {
    //     username: 'user1',
    //     id: 3,
    //     roles: ['user'],
    //     token: 'xxxxxxxxxxxxxxxx'
    // }
    hasura.auth.logout(onSuccess, onError);
    hasura.user // will be reset to anonymous user

Data query
~~~~~~~~~~

**Option 1:**

Use lambdas or anonymous functions directly for handling success/error.

.. code:: javascript

    hasura.data.query({
      type: 'select',
      args: {
        table: 'article',
        columns: ['*']
      }},
      (data) => { console.log(data); },
      (error) => { console.log(error); }
    );

**Option 2:**

Use predefined functions as shown below:

.. code:: javascript

    function mySuccessHandler (data) {
      console.log(data);
    }
    
    function myErrorHandler (e) {
      console.log(e);
    }
    
    hasura.data.query({
      type: 'select',
      args: {
        table: 'article',
        columns: ['*']
      }},
      mySuccessHandler,
      myErrorHandler
    );

Data query-templates
~~~~~~~~~~~~~~~~~~~~

**NOTE**: In the examples below, onSuccess and onError are callback functions that you must implement.

.. code:: javascript

    // This will use the hasura.user session object to send
    // if hasura.user.token === null, then request is made as an anonymous user (no auth token)
    hasura.data.queryTemplate(
      'query-template-name',
      {
      param: <value>,
      param2: <value2>
      },
      onSuccess,
      onError);

    // Query with a specific role
    hasura.data.queryTemplateAsRole(
      'user',
      'query-template-name',
      {
      param: <value>,
      param2: <value2>
      },
      onSuccess,
      onError);

Filestore usage
~~~~~~~~~~~~~~~

The Hasura JS SDK provides convenience functions to upload and download files.

.. code:: javascript

    <input id="my-file" type="file" />
    var fileInput = document.getElementById('my-file');
    var fileId;
    hasura.file.upload(
      fileInput,
      (successResponse) => {
        fileId = successResponse.file_id;
        console.log('Uploaded file: ' + fileId);
        // your code goes here
      },
      (errorResponse) => {
        console.log('Error uploading file');
        console.log(errorResponse);
        // your code goes here
      });
    
    hasura.file.download(fileId); // This will use the HTML5 download attribute to start downloading the file
    
    hasura.file.delete(fileId);

API requests to custom APIs deployed on Hasura
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Hasura JS SDK provides a simple wrapper over fetch to make it easy for you to make API requests to APIs deployed as custom microservices on Hasura.

**If you're making a JSON request:**

.. code:: javascript

    hasura.fetch(
    {
      microservice: 'api',  // the name of your custom microservice
      path: '/submit', // the path
      method: 'POST',  // HTTP method (this is POST by default, so you can ignore this key if it's POST)
      json: {...},     // set this to an object or an array that will be serialised to make the request body
      headers: {
        'X-Custom-Header': '...'
      }
    },
    (jsonResponse) => {
      // your success handler function
      console.log(jsonResponse);
      
      // By the way, jsonResponse is an object or an array
      // if the response content-type is application/json
      console.assert(typeof(jsonResponse) === 'object');
    },
    (error) => {
      // your error handler function
      console.error(error);
    });

**If you're making a request with a non JSON content-type:**

.. code:: javascript

    hasura.fetch(
    {
      microservice: 'api',  // the name of your custom microservice
      path: '/submit', // the path
      method: 'POST',  // HTTP method (this is POST by default, so you can ignore this key if it's POST)
      body: '...',     // set this to a string or a serialised value
      headers: {
        'Content-Type': '...' // you must set the content-type, because the default content-type is set to application/json
      }
    },
    (response) => {
      // your success handler function
      console.log(response);
    },
    (error) => {
      // your error handler function
      console.error(error);
    });

Other SDKs
----------
Web SDKs in other languages are currently WIP. If you want an early preview of or want to contribute to these SDKs, please write to us at support@hasura.io.
