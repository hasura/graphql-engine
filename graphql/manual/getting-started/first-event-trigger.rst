Making your first event trigger
===============================

Let's create a sample event trigger on table, with https://httpbin.org as our simple webhook.

Create table
------------
Head to the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with
the following columns:

+----------+----------+
|   **profile**       |
+----------+----------+
| id       | integer  |
+----------+----------+
| name     | text     |
+----------+----------+

.. image:: ../../../img/graphql/manual/getting-started/create-profile-table.png


Setup an event trigger
----------------------
In the Hasura console, navigate to ``Events -> Create trigger`` and:

1. Enter trigger name as ``echo``
2. Select table ``profile`` from the table dropdown
3. Select operations: ``insert``, ``update`` and ``delete``
4. Enter webhook URL as: ``https://httpbin.org/post``

.. image:: ../../../img/graphql/manual/getting-started/create-event-trigger.png

This sets up our webhook ``https://httpbin.org/post`` to receive database changes on insert, update and delete.


Insert some data
----------------
