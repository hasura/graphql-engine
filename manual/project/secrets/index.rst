.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-secrets-manual:

.. highlight:: bash

Project secrets
===============

Project secrets are variables that your microservices need, but variables that are sensitive and not safe to put in your git repository.
Here are some typical examples:

- Postgres (database) password
- API tokens for 3rd party APIs

Hasura gives you a simple way of managing your secrets directly on your cluster (via Kubernetes secrets) which
can then be passed as ENV variables to the microservices that need them.

See:
^^^^
.. toctree::
   :maxdepth: 1
   :titlesonly:

   Adding/updating secrets <add>
   Listing secrets <list>
   Passing secrets as ENV <passing-as-env>
