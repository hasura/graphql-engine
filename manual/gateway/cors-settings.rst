CORS Settings
==============

Browsers enforce the `same-origin policy <https://en.wikipedia.org/wiki/Same_origin_policy>`_ which prevents JavaScript from making requests across domain boundaries. By default, the gateway has CORS policy which is configured to allows requests from ``*.<cluster-name>.hasura-app.io``.

Sometimes during development we may want to let the gateway allow requests from js on some app running locally. 

To do this, head to your Hasura project folder. Inside conf/routes.yaml, there is a corsPolicy configuration.

Can take the following 3 values:

1. "allow_all": Cross origin requests from any domain are allowed
   Eg. corsPolicy: allow_all

2. "upstream" : The upstream service should handle CORS requests.
   Eg. corsPolicy: upstream

3. Array of allowed origins: This allows the listed origins along
   with all the subdomains on the current domain to make CORS requests.

For an app running locally, this is usually something like localhost:3000 (note that the port is necessary, wherever your local app is running) and this would come under array of allowed origins.

Example:

.. code-block:: yaml

api:
  /:
    upstreamService:
      name: auth
      namespace: {{ cluster.metadata.namespaces.hasura }}
    upstreamServicePath: /
    upstreamServicePort: 80
    corsPolicy: allow_all
