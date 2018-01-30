CORS Settings
==============

Browers enforce the `same-origin policy <https://en.wikipedia.org/wiki/Same_origin_policy>`_ which prevents JavaScript from making requests across domain boundaries. By default, the gateway has CORS policy which is configured to allows requests from ``*.<cluster-name>.hasura-app.io``.

Sometimes during development we may want to let the gateway allow requests from js on some app running locally. To do this, head to Console > API Gateway > Settings > microservice. Select custom origins and add the domain you would like to allow. For an app running locally, this is usually something like localhost:3000 (note that the port is necessary, wherever your local app is running).
