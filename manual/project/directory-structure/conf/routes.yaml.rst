.. _hasura-dir-conf-routes.yaml:

Project structure: conf/routes.yaml
===================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

This section lets you define the mapping between HTTP(S) routes on your domain(s) and
the microservices running on your cluster

The standard convention is to expose a microservice on a subdomain. The
default services (data, auth, filestore, notify etc.) on the platform are
exposed on their respective subdomains.

The configuration syntax is fairly simple. It is a simple map, where the keys
are subdomains, while the values are the corresponding subdomain configuration

So, the structure of this file looks like this:

.. code-block:: yaml

   subdomain1: subdomain1 configuration
   subdomain2: subdomain2 configuration
   .
   .
   subdomainN: subdomainN configuration

Subdomain's Configuration
-------------------------

Each subdomain's configuration is again a map. This time the keys are
location (path) prefixes and the values are the corresponding location
configuration. This lets you define the action that the gateway needs
to take when the location is matched.

Expanding the above conf structure, we get

.. code-block:: yaml

   subdomain1:
     location1: location configuration
     location2: location configuration
     .
     .
     locationN: locationN configuration

A location/path is the part of the url after the domain. For example:
In https://data.hello99.hasura-app.io/v1/query, ``/v1/query`` is the path

Example 1:
~~~~~~~~~~

.. code-block:: yaml

   data:
     /:
       upstreamService:
         name: data
         namespace: {{ cluster.metadata.namespaces.hasura }}
       upstreamServicePath: /
       upstreamServicePort: 80

Here, we are configuring the gateway to send any request on data
subdomain to the 'data' microservice in the hasura  namespace if
the location has a prefix '/'. Since every location starts with
'/', this means that any request arriving on the data subdomain
is received by the data microservice.

Example 2:
~~~~~~~~~~

.. code-block:: yaml

   api:
     /shipping:
       upstreamService:
         name: shipping
         namespace: {{ cluster.metadata.namespaces.user }}
       upstreamServicePath: /
       upstreamServicePort: 80
     /recommend:
       upstreamService:
         name: recommend
         namespace: {{ cluster.metadata.namespaces.user }}
       upstreamServicePath: /
       upstreamServicePort: 80

Here,

+---------------------------------------+---------------+
| api.hello99.hasura-app.io/shipping/*  | -> shipping/  |
+---------------------------------------+---------------+
| api.hello99.hasura-app.io/recommend/* | -> recommend/ |
+---------------------------------------+---------------+

.. note::

   1. Since the convention is to deploy a microservice on each domain, you'll rarely
      see a configuration which has a location prefix other than '/'

   2. If in case the location matches to more than one rule, the configuration
      related to the more specific rule is used

Location Configuration
----------------------

The following options are available:

+---------------------+----------+---------+
| key                 | required | default |
+=====================+==========+=========+
| upstreamService     | Yes      |         |
+---------------------+----------+---------+
| upstreamServicePort | Yes      |         |
+---------------------+----------+---------+
| upstreamServicePath | Yes      |         |
+---------------------+----------+---------+
| enableAuth          | No       | true    |
+---------------------+----------+---------+
| authorizationPolicy | No       | null    |
+---------------------+----------+---------+
| corsPolicy          | No       | []      |
+---------------------+----------+---------+
| enableWebsockets    | No       | true    |
+---------------------+----------+---------+
| locationDirectives  | No       | ""      |
+---------------------+----------+---------+

- upstreamService:
    The service to forward the request to

- upstreamServicePort:
    The port on which the service is running

- upstreamServicePath:
    The path to which the request has to be forwarded

- enableAuth:
    This enables the session middleware on the gateway to intercept the
    request and resolve the user's session based on Authorization header
    or the Cookie

- authorizationPolicy:
    This can be used to restrict access to a microservice when the  microservice
    is not aware of users or roles. For example, let's say you want to run some
    analytics service (pghero) that should only be allowed for admins:

    .. code-block:: yaml

       pghero:
         /:
           upstreamService:
             name: pghero
             namespace: {{ cluster.metadata.namespaces.user }}
           upstreamServicePath: /
           upstreamServicePort: 80
           authorizationPolicy:
             restrictToRoles: ["admin"]

    With this conf, only logged in users with the role admin can access 'metabase' service

    Now we need a need a way for the admins to login so that they can access this service.
    The auth service has a ui-kit that can be used for this purpose.

    .. code-block:: yaml

       pghero:
         /:
           upstreamService:
             name: pghero
             namespace: {{ cluster.metadata.namespaces.user }}
           upstreamServicePath: /
           upstreamServicePort: 80
           authorizationPolicy:
             restrictToRoles: ["admin"]
             noSessionRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/

    So if there is no session, the gateway redirects the user to the auth service's ui to login.

    Sometimes, additionally we may want to redirect users which do not have access to this page

    .. code-block:: yaml

      pghero:
        /:
          upstreamService:
            name: pghero
            namespace: {{ cluster.metadata.namespaces.user }}
          upstreamServicePath: /
          upstreamServicePort: 80
          authorizationPolicy:
            restrictToRoles: ["admin"]
            noSessionRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/
            noAccessRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/restricted

- corsPolicy:
    Can take the following 3 values:

    1. "allow_all": Cross origin requests from any domain are allowed
       Eg. corsPolicy: allow_all

    2. "upstream" : The upstream service should handle CORS requests.
       Eg. corsPolicy: upstream

    3. Array of allowed origins: This allows the listed origins along
       with all the subdomains on the current domain to make CORS requests.

- enableWebsockets:
    Whether to allow websockets

- locationDirectives:
    (Advanced) Additional nginx directives that need to go into the
    location block

You can find the default file at `conf/routes.yaml <https://github.com/hasura/base/blob/master/conf/routes.yaml>`_ in the base repo.
