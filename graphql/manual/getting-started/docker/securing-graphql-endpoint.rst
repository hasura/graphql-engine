Securing your GraphQL endpoint
==============================

To make sure that your Hasura GraphQL endpoint is not publicly accessible, you need to configure an access key.

Once we secure the endpoint, this is what will happen:

#. The console will not be served via the GraphQL engine anymore
#. The GraphQL engine will only allow access through an access key
#. A CLI will be used to access the console, which has the added benefit of automatically generating database :doc:`migrations<../../migrations/index>` that you can put in version control.

Step 1: Download the CLI
------------------------

Step 2: Configure the CLI to use your endpoint
----------------------------------------------

Step 3: Set an access key on the GraphQL engine
-----------------------------------------------

If you're looking at adding authentication and access control to your GraphQL API then head to :doc:`Authentication / access control <../auth/index>`.
