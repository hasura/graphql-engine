Part VII: Write your own Resolvers
==================================

You can customise GraphQL schema using views. But there are instances where you would like to write custom business logic, contacting third party APIs, returning data which is not stored in the database etc. In those cases, you would want to write your own resolver in GraphQL and expose an API endpoint.

The Hasura platform allows you to deploy custom code as :ref:`microservices <hasura_microservice_doc>` with a git push. Your microservices can act as a GraphQL proxy where you can add custom resolvers and then connect to Hasura GraphQL engine or any other third-party API.

GraphQL Server Boilerplate
--------------------------

We have a GraphQL `graphql-server-boilerplate <https://hasura.io/hub/projects/hasura/graphql-server-boilerplate>`_ that you can quickly add to your project and get your own GraphQL server live in under five minutes. It has a simple hello-world schema which looks like:

.. code-block:: none

    type Hello {
      world: String
      echo: String
      random: Int
      time: String
    }

.. code-block:: none

    type Query {
      hello(echo: String!): Hello
    }

The ``graphql-server-boilerplate`` exposes the GraphQL API at the ``/`` endpoint. It also servers ``GraphiQL`` at the ``/graphiql`` endpoint where you can try out the API.

Adding the GraphQL server boilerplate to your project
-----------------------------------------------------

Just run the following commands from your project directory to add this server boilerplate to your project as a microservice:

.. code-block:: bash

    # Clone the boilerplate code
    $ hasura microservice clone api --from hasura/graphql-server-boilerplate

    # Generate a remote for deploying the server with a git push
    $ hasura conf generate-remote api >> conf/ci.yaml

    # Generate a route to expose the server to the internet
    $ hasura conf generate-route api >> conf/routes.yaml

Finally deploy the server using a git push:

.. code-block:: bash

    $ git add .
    $ git commit -m "Added a graphql server to microservices"
    $ git push hasura master

You can try out this hello-world GraphQL API at ``https://api.<cluster-name>.hasura-app.io/graphiql``.

Modifying the source code
-------------------------

The source code for the server is in the ``microservices/api/src`` directory.

The resolver code looks like:

.. code-block:: javascript

    // resolvers
    const resolvers = {
        Query: {
            hello: (root, args, context, info) => {
                const respObj = {
                    'world': 'world',
                    'time': getTime(),
                    'random': getRandomNum(),
                    'echo': args.echo
                };
                return respObj;
            }
        },
    }

    // get timestamp
    function getTime(){
        const currentTime = new Date().getTime().toString();
        return currentTime;
    }

    // get a random number
    function getRandomNum(){
        const randomNum = Math.floor(Math.random() * 100) + 1;
        return randomNum;
    }

However, after you write and deploy a custom GraphQL server, you will end up with two endpoints: the Hasura GraphQL engine and your own custom server. Having a single endpoint for all the APIs is one of the major advantages of using GraphQL. 

Head over to the section that talks about merging two GraphQL endpoints into one i.e. :doc:`../data/graphql/schema-stitching`