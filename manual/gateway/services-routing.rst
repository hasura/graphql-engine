Services & Routing
==================

The API gateway is responsible for routing sub-domains to the correct upstream
microservice. It also terminates SSL connections.

For example, in the cluster ``test42.hasura-app.io``, if one makes a request
``https://auth.test42.hasura-app.io`` the gateway will terminate the SSL
connection, resolve sessions, and proxy the request to the upstream ``Auth``
microservice.

The routing is done via configuration, using the ``hasura`` CLI. Refer to
:doc:`../hasuractl/hasura_conf_generate-route` for more details.

Once the route configuration is applied, the gateway reads the config and
templates Nginx with the new route configuration.
