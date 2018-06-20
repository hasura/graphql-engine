Routing using API gateway
=========================

The API gateway is responsible for routing sub-domains to the correct upstream
microservice. It also terminates SSL connections.

For example, in the cluster ``test42.hasura-app.io``, if one makes a request
``https://auth.test42.hasura-app.io`` the gateway will terminate the SSL
connection, resolve sessions, and proxy the request to the upstream ``Auth``
microservice.

The routing configuration is stored in the ``conf/routes.yaml``
file in the project directory.

During deployment, the API gateway reads the ``routes.yaml`` file and
templates Nginx with the new route configuration.

Add a new route for a microservice:
-----------------------------------

A new route for a microservice can be added using the ``hasura`` CLI. See :doc:`../hasuractl/hasura_conf_generate-route`

Change subdomain of a microservice:
-----------------------------------

To change the subdomain a microservice is exposed at, see :doc:`../microservices/change-subdomain`

Detailed routing:
-----------------

For a detailed understanding of how you can customise your projects routing config, check out :doc:`../project/directory-structure/conf/routes.yaml`

