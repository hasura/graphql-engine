.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data
=====

The data service on Hasura exposes an HTTP/JSON API over a PostgreSQL database.
This API is designed to be used by any web client (speaking JSON), especially
frontend interfaces like Android and iOS apps, browser based JavaScript apps
etc.

The data API provides the following features:

1. CRUD APIs on PostgreSQL tables with a MongoDB-esque JSON query syntax.
2. Rich query syntax for making complicated select queries.
3. Role based access control at row and column level.

.. toctree::
  :maxdepth: 2

  api
  reference
