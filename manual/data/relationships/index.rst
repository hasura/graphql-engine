Data relationships
==================

There is usually some kind of association between tables in a database. These associations are typically captured
by foreign key constraints when the data is modelled. The data microservice lets you define "relationships" between
these tables.

For example, an ``article`` table might have a column called ``author_id`` which points to a row in the ``author`` table.
You may wish to fetch all the *articles* of an author when fetching from the ``author`` table, or fetch the *author* of an
article when fetching from the ``article`` table.

These additional properties, *articles* of an *author* and *author* of an *article*, are what we call relationships.
*articles* of *author* is an **array relationship** while *author* of *article* is an **object relationship**.

See:
^^^^
.. toctree::
   :maxdepth: 1

   Creating relationships <create-relationships>
   fetch-over-relationships
