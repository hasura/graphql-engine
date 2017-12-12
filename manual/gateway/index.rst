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


Services & Routing
------------------
The API gateway is responsible for routing sub-domains to the correct upstream
microservice. It also terminates SSL conections. 

For example, in the cluster ``test42.hasura-app.io``, if one makes a request
``https://auth.test42.hasura-app.io`` the gatway will terminate the SSL
connection, resolve sessions, and proxy the request to the upstream ``Auth``
microservice.

The routing is done via configuration, using the ``hasura`` CLI. Refer to
:doc:`../hasuractl/hasura_conf_generate-route` for more details.

Once the route configuration is applied, the gateway reads the config and
templates Nginx with the new route configuration.


Session Middleware
------------------
The gateway has a session middleware built into it. This means, for all
external requests (i.e requests coming from outside the cluster) the gateway
looks for a Cookie or an ``Authorization`` header, resolves a session based on
those information, and proxies the request to the correct upstream microservice
with the session information in special HTTP headers. Specifically, these
headers are ``X-Hasura-User-Id``, ``X-Hasura-Role`` and
``X-Hasura-Allowed-Roles``.

So an upstream microservice doesn't have to implement its own logic of
resolving session from the request. It can just read the special headers
forwarded by the gateway to determine which user made that request, what is the
role of the user etc. Based on this information it can then implement its own
authorization logic.

For logged in users
~~~~~~~~~~~~~~~~~~~
If the gateway could resolve a valid session from the cookie or
``Authorization`` header, then values of the ``X-Hasura-*`` headers will be:

* ``X-Hasura-User-Id`` : ``hasura_id`` of the user (e.g ``X-Hasura-User-Id:
  42``).
* ``X-Hasura-Role`` : Current role of the logged in user. (e.g ``X-Hasura-Role:
  user``).
* ``X-Hasura-Allowed-Roles`` : Comma separated values of all roles that user
  has (e.g ``X-Hasura-Allowed-Roles: user,admin``).


For non-logged in users
~~~~~~~~~~~~~~~~~~~~~~~
If the request does not contain any cookie or ``Authorization`` header, the
gateway will add anonymous values in the ``X-Hasura-*`` headers.

* ``X-Hasura-User-Id`` : Value will be ``0``.
* ``X-Hasura-Role`` : Value will be ``anonymous``.


Custom HTTP directives
----------------------
Custom Nginx HTTP directives can also be added to the gateway.

This can be done in ``conf/http-directives.yaml`` inside your project
directory. Then git-commit and git-push to apply these changes to a particular
cluster.

CORS Settings
-------------
Browers enforce the `Same-Origin policy
<https://en.wikipedia.org/wiki/Same_origin_policy>`_ which prevents JavaScript
from making requests across domain boundaries. By default, the gateway has CORS
policy which is configured to allows requests from
``*.<cluster-name>.hasura-app.io``.

Sometimes during development we may want to let the gateway allow requests from
JavaScript on some app running locally. 

To do this, edit the ``conf/routes.yaml`` file and edit the ``corsPolicy``
settings. For an app running locally, this is usually something like
localhost:3000 (note that the port is necessary, wherever your local app is
running). Then git-commit and git-push to apply these changes to a particular
cluster.

.. _custom-domains:

Custom domains & SSL
--------------------
Your Hasura cluster comes with a SSL enabled `hasura-app.io` domain. You can
also point your own domain to the cluster so that your microservices/website is
available on the domain.

Hasura provisions free SSL certificates for each domain you add using
`LetsEncrypt <https://letsencrypt.org/>`_. All of your microservices will be
available on each of these domains.

Adding a custom domain
~~~~~~~~~~~~~~~~~~~~~~

- Get IP for the cluster

.. code-block:: bash

   $ ping cluster-name.hasura-app.io

- Point your domain's DNS to the cluster's IP from your registrar's dashboard by adding a A record for your domain pointing to the IP above

+---+----------------+---------+
| A | `*.domain.com` | 1.1.1.1 |
+---+----------------+---------+

- Goto ``conf/domains.yaml`` and add the following block to the file where ``domain.com`` is your domain:

.. code-block:: yaml

   domain.com:
     ssl:
       conf:
         account: you@youremail.com
       type: LetsEncrypt


- Apply your changes to the cluster

.. code-block:: bash

   $ hasura conf apply # add -c <cluster-name>, in case you have multiple clusters

Now, SSL certificates will automatically be generated and your microservices
will be accessible on this domain!
