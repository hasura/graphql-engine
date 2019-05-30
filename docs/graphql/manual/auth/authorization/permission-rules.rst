Configuring Permission Rules
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 3
  :local:

Introduction
------------

Access control rules in Hasura are defined at a role, table and action (*insert, update, select, delete*)
level granulaity:

.. thumbnail:: ../../../../img/graphql/manual/auth/permission-rule-granularity.png

Requests to Hasura should contain the reserved session variable ``X-Hasura-Role`` to indicate the requesting
user's role, and the table and action information is inferred from the request itself. This information is used
to determine the right permission rule to be applied (*if one has been defined*) to the incoming request.

Hasura converts GraphQL queries (*or mutations/subscriptions*) into a single SQL query that is executed on the
configured database instance.
Hasura also includes constraints from permission rules in the SQL query itself.

Permissions are essentially a combination of **boolean expressions** and **column selections** that impose
constraints on the data being returned or modified.

Let's take a look at the different configuration options available to define a permission rule. Permission
rules are defined for each role, table, operation (*insert, select, update, delete*) by using the console
or the :doc:`metadata APIs for permissions <../../api-reference/schema-metadata-api/permission>`.

Operation permissions
---------------------

**Select** permissions
^^^^^^^^^^^^^^^^^^^^^^
For ``select`` operations or for GraphQL queries, you can configure the following:

* :ref:`row-level-permissions`

* :ref:`col-level-permissions`

* :ref:`aggr-query-permissions`

* :ref:`limit-rows-permissions`


**Insert** permissions
^^^^^^^^^^^^^^^^^^^^^^
For ``insert`` operations or for GraphQL mutations of the type *insert*, you can configure the following:

* :ref:`row-level-permissions`

* :ref:`col-level-permissions`

* :ref:`col-presets-permissions`

**Update** permissions
^^^^^^^^^^^^^^^^^^^^^^
For ``update`` operations or for GraphQL mutations of the type *update*, you can configure the following:

* :ref:`row-level-permissions`

* :ref:`col-level-permissions`

* :ref:`col-presets-permissions`

**Delete** permissions
^^^^^^^^^^^^^^^^^^^^^^
For ``delete`` operations or for GraphQL mutations of the type *delete*, you can configure the following:

* :ref:`row-level-permissions`

Permission categories
---------------------

.. _row-level-permissions:

Row-level permissions
^^^^^^^^^^^^^^^^^^^^^

Row-level premissions are **boolean expressions** that help you restrict access to rows depending on the
operation being performed. E.g. in the case of ``select``, your boolean expression is run on every row to
determine whether that row can be read. In the case of ``insert``, the boolean expression determines whether or not the mutation is allowed.

Row-level permissions are defined using operators, static values, values in columns (*including those in
related tables or nested objects*) and session variables.

Using operators to build rules
******************************

Type-based operators (*depending on the column type*) are available for constructing row-level permissions.
You can use the same operators that you use to :doc:`filtering query results <../../queries/query-filters>`
to define permission rules.

E.g. the following two images illustrate the different operators available for ``integer`` and ``text`` types:


.. thumbnail:: ../../../../img/graphql/manual/auth/operators-for-integer-types.png

.. thumbnail:: ../../../../img/graphql/manual/auth/operators-for-text-types.png

Using boolean expressions
*************************

The following is an example of a simple boolean expression to restrict access for ``select`` to rows where
the value in the ``id`` column is greater than 10:

.. thumbnail:: ../../../../img/graphql/manual/auth/simple-boolean-expression.png

You can construct more complex boolean expression using the ``_and``, ``_or`` and ``not`` operators:

.. thumbnail:: ../../../../img/graphql/manual/auth/boolean-operators.png

E.g. using the ``_and`` operator, you can construct a rule to restrict access for ``select`` to rows where
the value in the ``id`` column is greater than 10 **and** the value in the ``name`` column starts with "a"
or "A":

.. thumbnail:: ../../../../img/graphql/manual/auth/composite-boolean-expression.png

Using session variables
***********************

Session variable, that have been resolved from authentication tokens by either your authentication webhook or
by Hasura using the JWT configuration, are available for constructing row-level permissions.

E.g. to allow an ``author`` to access only their articles, you can use the ``X-Hasura-User-ID`` session variable
to construct a rule to restrict access for ``select`` to rows in the ``articles`` table where the value in the
``id`` column is equal to the value in the session variable (*assuming this variable is being used to indicate
the author's ID*):

.. thumbnail:: ../../../../img/graphql/manual/auth/session-variables-in-permissions-simple-example.png

.. _relationships-in-permissions:

Using relationships or nested objects
*************************************

You can leverage :doc:`relationships <../../schema/relationships/index>` to define permission rules with fields
from a nested object.

For e.g. let's say you have an object relationship called ``agent`` from the ``authors`` table to another table
called ``agent`` (*an author can have an agent*) and we want to allow users with the role ``agent`` to access
the details of the authors who they manage in ``authors`` table. We can define the following permission rule
that uses the aforementioned object relationship:

.. thumbnail:: ../../../../img/graphql/manual/auth/nested-object-permission-simple-example.png

This permission rule reads as "*if the author's agent's*  ``id``  *is the same as the requesting user's*
``id`` *, allow access to the author's details*.


.. admonition:: Array and Object relationships work similarly

   - The above example would have worked even if the relationship were an array relationship. In our example,
     the corresponding rule for an array relationship would have read "*if any of this author's agents'* ``id``
     *is the same as the requesting user's* ``id`` *, allow access to the author's details*".

   - You can also check out this more elaborate :ref:`example<nested-object-permissions-example>`.


.. _col-level-permissions:

Column-level permissions
^^^^^^^^^^^^^^^^^^^^^^^^
Column-level permissions determine access to columns in the rows that accessible based on row-level
permissions. These permissions are simple selections:

.. thumbnail:: ../../../../img/graphql/manual/auth/column-level-permissions.png

In this example, the role ``author`` has only partial access to columns of the accessible rows for
the ``select`` operation.

.. _limit-rows-permissions:

Row Fetch Limit
^^^^^^^^^^^^^^^

In the case of ``select`` operations, the number of rows to be returned in the response can be limited
using this configuration:

.. thumbnail:: ../../../../img/graphql/manual/auth/limit-rows-for-select.png

In the above example, this configuration  restricts the number of accessible rows (*based on the rule*:
``{"id":{"_eq":"X-Hasura-User-Id"}}``) to 20.

.. _aggr-query-permissions:

Aggregation queries permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the case of ``select`` operations, access to :doc:`aggregation queries <../../queries/aggregation-queries>`
can be restricted for a given role using this configuration.

.. thumbnail:: ../../../../img/graphql/manual/auth/aggregation-query-permissions.png

In the above example, the role ``user`` is allowed to make aggregation queries.

.. _col-presets-permissions:

Column presets
^^^^^^^^^^^^^^

While this is strictly not a permission configuration, defining
:doc:`role-based column presets <../../schema/default-values/column-presets>` on any column automatically
removes access to it. This preset can be defined for ``insert`` and ``update`` operations. This configuration
is also very useful to avoid sending sensitive user-information in the query and leverage session variables
or static data instead.
