===============================
Adding a foreign key constraint
===============================

If many records will refer to the same data, it is more efficient and less error prone to update a single record and keep references to it in other places. That's where foreign key constraints come in. The foreign key constraint will ensure consistency of data and no overheads on update.

In this section, we will show how to add a foreign key constraint over a column. Consider a sample schema with an ``article`` and an ``author`` table. Lets try to add a foreign key constraint over the ``author_id`` column of the ``article`` table.

#. Open the API Console and go to to the Data Tab.
#. Choose the ``article`` table on the left panel.
#. Click on the ``Modify`` tab.
#. Click on the column name that you wish to add a foreign key constraint over (in this case, ``author_id``).
#. Check the ``Foreign key`` checkbox and choose your reference table and reference column (in this case, ``author`` and ``id`` respectively).
#. Hit ``Save``.

.. image:: img/adding-fk.png
