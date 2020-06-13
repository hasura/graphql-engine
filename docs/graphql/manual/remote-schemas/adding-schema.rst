.. meta::
   :description: Add a remote schema with Hasura
   :keywords: hasura, docs, remote schema, add

.. _adding_schema:

Adding a remote schema  
----------------------

Follow the steps below to add a "remote schema" to the Hasura GraphQL engine.

Step 1: Write a custom GraphQL server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You need to create a custom GraphQL server with a schema and corresponding resolvers that solve your use case
(*if you already have a functional GraphQL server that meets your requirements, you can skip this step*).

You can use any language/framework of your choice to author this server and deploy it anywhere. A great way to get
started is to use one of our boilerplates:

- `Boilerplates <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas>`__

.. _merge_remote_schema:

Step 2: Merge remote schema
^^^^^^^^^^^^^^^^^^^^^^^^^^^

To merge your remote schema with the GraphQL engine's auto-generated schema:

Head to the ``Remote Schemas`` tab of the console and click on the ``Add`` button.

.. thumbnail:: /img/graphql/manual/business-logic/add-remote-schemas-interface.png
   :alt: Merge remote schema

You need to enter the following information:

- **Remote Schema name**: an alias for the remote schema that must be unique on an instance of the GraphQL engine.
- **GraphQL server URL**: the endpoint at which your remote GraphQL server is available. This value can be entered
  manually or by specifying an environment variable that contains this information.

  .. note::

    During **local development** with the remote schema server on localhost, ensure that the Hasura Docker container can reach
    the server endpoint on the host. A common way to do this is to use the domain ``host.docker.internal`` on Docker for Mac and Windows or the Docker bridge IP on Linux (typically ``172.17.0.1`` - the IP address of ``docker0`` interface. use ``ip addr show docker0``) instead of ``127.0.0.1``. 
    
    Similarly, if you are adding the URL using env variable, then run the Hasura docker container with the env variable added during `docker run`. Example ``-e REMOTE_SCHEMA_ENDPOINT=http://host.docker.internal:4000/mycustomgraphql``

- **Headers**: configure the headers to be sent to your custom GraphQL server:

  - Toggle forwarding all headers sent by the client (when making a GraphQL query) to your remote GraphQL server.
  - Send additional headers to your remote server - these can be static header name-value pairs; and/or pairs of
    "header name-environment variable name". You can specify the value of the header to be picked up from the environment
    variable.

    **Example**: Let's say your remote GraphQL server needs a ``X-Api-Key`` as a header. As this value contains
    sensitive data (like API key in this example), you can configure the name of an environment variable which will hold
    the value. This environment variable needs to be present when you start the GraphQL engine. When Hasura sends
    requests to your remote server, it will pick up the value from this environment variable.

.. admonition:: Using environment variables

  If you are using environment variables in the remote schema configuration - either
  for URL or headers - **the environment variables need to be present**  with valid values
  when adding the remote schema i.e. the GraphQL engine should be started with these environment variables.

Click on the ``Add Remote Schema`` button to merge the remote schema.

Step 3: Make queries to the remote server from Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Now you can head to the ``GraphiQL`` tab and make queries to your remote server from Hasura.

You can query your remote server by making requests to the Hasura GraphQL endpoint (``/v1/graphql``).