.. .. meta::
   :description: Hasura API gateway manual
   :keywords: hasura, api gateway, routing, session middleware, cors, custom domains, domains, ssl


Hasura API Gateway
==================

The Hasura ``Gateway microservice`` consists of a custom Lua scripted Nginx.

It is responsible for routing all external requests to the correct upstream
(acting as a reverse proxy), terminating SSL connections and resolving sessions
for upstream microservices (acting as a session middleware).

See:
^^^^

.. toctree::
   :maxdepth: 2
   :titlesonly:

   Routing <routing>
   Gateway as Session Middleware <session-middleware>
   custom-http-directives
   custom-domains-ssl
   cors-settings
