FAQ
---

How do I make aggregation or analytics queries?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can use SQL from your business logic code.

How do I use custom Postgres queries or operators not supported by the JSON query language?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can use SQL from your business logic code.

Who has access to the data? How do I define access control?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
By default, only the ``admin`` role will have access to the data. You can
define permissions to give access to other roles.

How do I import/export data?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can use ``psql`` or ``pgdump`` to import/export data.

Can I access the underlying Postgres to make raw SQL queries?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Yes.

How do I import/export data from/to other databases?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can use standard database and PostgreSQL tools.


How can I use hierarchical permissions?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Can the same user use the data APIs using multiple permissions?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

What if I delete or alter columns that are being used in the permission definition?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

How do I version control my permission definitions along with my schema?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Can I create relationships on these views?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

What if I delete/alter a view?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

How do I version control the SQL required for these views?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
