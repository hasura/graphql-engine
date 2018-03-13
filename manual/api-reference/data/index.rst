.. .. meta::
   :description: Homepage of the reference documentation for Hasura's Data  microservice and a Table of Contents
   :keywords: hasura, docs, data, home, toc, table of contents

Data API Reference
==================

The data microservice on Hasura exposes an HTTP/JSON API over a PostgreSQL database.
This API is designed to be used by any web client (speaking JSON), especially
frontend interfaces like Android and iOS apps, browser based JavaScript apps
etc.

The data API provides the following features:

1. CRUD APIs on PostgreSQL tables with a MongoDB-esque JSON query syntax.
2. Rich query syntax for making complicated select queries.
3. Role based access control at row and column level.

See:
^^^^

.. toctree::
  :maxdepth: 2

  Query endpoints <endpoints>
  Queries <queries>
  FAQ <faq>
