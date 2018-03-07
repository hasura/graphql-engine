.. .. meta::
   :description: Hasura API gateway manual
   :keywords: hasura, api gateway, routing, session middleware, cors, custom domains, domains, ssl


API Gateway
===========

The Hasura API Gateway (or simply the gateway) is a microservice consisting of
a custom Lua scripted Nginx, a Let's Encrypt agent and a Redis instance.

It is responsible for routing all external requests to the correct upstream
(acting as a reverse proxy), terminating SSL connections and resolving sessions
for upstream microservices (acting as a session middleware).

See:
^^^^

.. toctree::
   :maxdepth: 1

   Routing <routing>
   Session Middleware <session-middleware>
   Custom HTTP directives <custom-http-directives>
   custom-domains-ssl
   cors-settings
