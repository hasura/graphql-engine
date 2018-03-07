.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data Modelling Guide
====================

Don't duplicate data
--------------------

Storing duplicate data is a data management problem. Maintenance is a big overhead that you do not want to take up. It is recommended that you only store the references, and query the references when you need to retrieve information. Avoid maintaining duplicate data; it's complex and error-prone.

Add constraints for relationships
---------------------------------

If many records will refer to the same data it is more efficient and less error prone to update a single record and keep references to it in other places. That's where foreign key constraints come in. The foreign key constraint will ensure consistency of data and no overheads on update.

Derive Views
------------

- Abstraction
The relational data queries can be quite complex sometimes, leading to many joins or calculations. Instead the same complex query can be written as a view and the application can make a simpler query to the view instead of the table with joins.
Column names can be aliased to make it suitable for the application needs, abstracting the original table.

- Permissions
Views are also used to apply granular permissions for underlying tables. Views can be made accessible to users while the underlying tables are not directly accessible. This allows the DBA to give users only the data they need, while protecting other data in the same table. Hasura lets you define permissions for views.

- Legacy Code & Database Refactoring
Views help place logic in a single location, so that you do not have to change it all over the code base. Suppose you make a modification to your underlying schema, your application can still keep using the same query, provided the view is also aligned to the data structure. Eventually though, you might have to make the changes in your code base if it gets too complex.


Embed Permissions in your modelling
-----------------------------------

When you are designing data models, you have to reason about who is going to access the data and what are the privileges they are granted. Views can also abstract the original tables and the need for permissions to those tables.
Hasura Data API allows permissions to be defined for row-level and column-level granularity. All tables are admin-only by default. So you have to define roles based on your application requirement and assign the users appropriate roles for access control.

Migrations in Version Control
-----------------------------

Migration action can be either up (go forward) or down (go backward). A database can easily bring itself up to date by running the migrations that are in version control. Migrations are designed to be committed in version control (git) and distributed across developers building the application.

At any point of time, the application developer should be able to go back to an old feature tied to the data model, apply the migrations necessary and get it working (even if it means doing manually). Sometimes going back cannot be automated due to the complexity of the data models, but maintaining a history of schema changes will ensure that it can be done at least manually in complex cases.

Migrations should never be created in production, should only be applied. Hence testing the migrations in a staging environment is crucial before going to production.

Introduction to Data Modelling with PostgreSQL
----------------------------------------------

- Tables & Columns

A database consists of one or more tables. A table consists of rows and columns. Each row in a table is uniquely identified by the primary key. A table can contain zero or more rows. A column is defined by its name and its data type. Columns are designed to hold specific type of data (for example - integers, strings). 

- Column Types

PostgreSQL supports many data types:

1. Boolean
2. Character (char, varchar, text)
3. Numeric (integer and float)
4. Temporal (date, time, timestamp)
5. JSON

- Constraints on the table

Constraints are rules enforced on the columns of a table. This ensures data consistency and prevents invalid data from being entered into the table.

PostgreSQL supports following constraints:

1. Check Constraints - It ensures all values in a column satisfies a condition.
2. Not-Null Constraints - It ensures that a column cannot have a NULL value.
3. Unique Constraints - It ensures that all values in a column are different.
4. Primary Keys - Uniquely identifies each row in a table.
5. Foreign Keys - It constraints data of a column based on columns in other tables. We will talk about this in detail below.

- Permissions


Relationships
-------------

- Foreign Keys

A foreign key constraint specifies that the values in a column (or a group of columns) must match the values appearing in some row of another table. This maintains the referential integrity between two related tables.

Let's consider a simple scenario where there are two tables ``article`` and ``author``. The ``article`` table (child) has a column ``author_id`` which points to the ``id`` column of ``author`` table (parent). The foreign key here ensures that, every article has been written by an author who exists in the author table. If not for the foreign key reference, there could be articles with authors who may not exist in your database.

- Hasura Relationships

Relationships are used to capture the connectedness of data amongst tables. In a relational database, when modelling data we add foreign key constraints to establish connections between various tables.

In the above example of foreign key constraint, there are two possible scenarios.
When we fetch a row from the article table, we may need the author information along with article columns. 
When we fetch a row from the author table, we may need the articles written by the author along with the author columns.

So we can define the following relationships for the above scenarios.

1. ``author`` in ``article`` table
2. ``articles`` in ``author`` table

- Examples (many-to-one, many-to-many)