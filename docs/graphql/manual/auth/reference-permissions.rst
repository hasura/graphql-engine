Reference - Permission Rules
============================

Reference documentation for access control permissions.

.. contents:: Table of contents
  :backlinks: none
  :depth: 3
  :local:

How permissions work
--------------------

Hasura converts GraphQL queries (*or mutations/subscriptions*) into a single SQL query that is executed on the configured database instance. 
Hasura also includes constraints from role-based access control permission rules in the SQL query itself. Let's see an example to understand this in more detail.

Let's say you want to make the following simple query:

.. code-block:: graphql

    query {
      authors {
        id
        name
      }
    }

If you make this query as an admin in the Graphiql section of the console and click on the **Analyze** button, you will see following generated SQL:

.. code-block:: SQL
   
    SELECT
      coalesce(json_agg("root"), '[]') AS "root"
    FROM
    (
      SELECT
        row_to_json(
          (
            SELECT
              "_1_e"
            FROM
              (
                SELECT
                  "_0_root.base"."id" AS "id",
                  "_0_root.base"."name" AS "name"
              ) AS "_1_e"
          )
        ) AS "root"
      FROM
        (
          SELECT
            *
          FROM
            "public"."authors"
          WHERE
            ('true')
        ) AS "_0_root.base"
    ) AS "_2_root"

Let's say you now define the following permission rule on the ``authors`` table (*as described* :doc:`here <basics>`):

.. thumbnail:: ../../../img/graphql/manual/auth/permission-basics-simple-example.png

If you were to run the same query with the role ``user`` (``X-Hasura-Role`` = ``user``) and as a user with ``id`` = ``1`` (``X-Hasura-User-ID`` = ``1``) and hit the **Analyze** button, you will see the following generated SQL:

.. code-block:: SQL
   :emphasize-lines: 27-32

    SELECT
      coalesce(json_agg("root"), '[]') AS "root"
    FROM
      (
        SELECT
          row_to_json(
            (
              SELECT
                "_1_e"
              FROM
                (
                  SELECT
                    "_0_root.base"."id" AS "id",
                    "_0_root.base"."name" AS "name"
                ) AS "_1_e"
            )
          ) AS "root"
        FROM
          (
            SELECT
              *
            FROM
              "public"."authors"
            WHERE
              (
                (
                  ("public"."authors"."id") = (
                    (
                      (
                        current_setting('hasura.user') :: json ->> 'x-hasura-user-id'
                      )
                    ) :: integer
                  )
                )
                OR (
                  (("public"."authors"."id") IS NULL)
                  AND (
                    (
                      (
                        (
                          current_setting('hasura.user') :: json ->> 'x-hasura-user-id'
                        )
                      ) :: integer
                    ) IS NULL
                  )
                )
              )
          ) AS "_0_root.base"
      ) AS "_2_root"

As you can see from the highlighted section above, the access control constraints are included in the SQL query itself.

Configuring permissions
-----------------------
Permissions are essentially a combination of **boolean expressions** and **column selections** that impose constraints on the data being returned or modified.

Let's take a look at the different configuration options available to define a permission rule. Permission rules are defined for each role, table, operation (*insert, select, update, delete*) by using the console or the :doc:`metadata APIs for permissions <../api-reference/schema-metadata-api/permission>`.

We'll list the different configuration operations available for defining permission rules and then take a look at how they're applicable to different operations. 

.. _row-level-permissions:

Row-level permissions
^^^^^^^^^^^^^^^^^^^^^

Row-level premissions are **boolean expressions** that help you restrict access to rows depending on the operation being performed. E.g. in the case of ``select``, your boolean expression is run on every row to determine whether that row can be read. In the case of ``insert``, the boolean expression determines whether or not the mutation is allowed.

Row-level permissions are defined using operators, static values, values in columns (*including those in related tables or nested objects*) and session variables.

**Using operators to build rules**
##################################

Type-based operators (*depending on the column type*) are available for constructing row-level permissions. You can use the same operators that you use to :doc:`filtering query results <../queries/query-filters>` to define permission rules.

E.g. the following two images illustrate the different operators available for ``integer`` and ``text`` types:


.. thumbnail:: ../../../img/graphql/manual/auth/operators-for-integer-types.png

.. thumbnail:: ../../../img/graphql/manual/auth/operators-for-text-types.png

**Using boolean expressions**
#############################

The following is an example of a simple boolean expression to restrict access for ``select`` to rows where the value in the ``id`` column is greater than 10:

.. thumbnail:: ../../../img/graphql/manual/auth/simple-boolean-expression.png

You can construct more complex boolean expression using the ``_and``, ``_or`` and ``not`` operators:

.. thumbnail:: ../../../img/graphql/manual/auth/boolean-operators.png

E.g. using the ``_and`` operator, you can construct a rule to restrict access for ``select`` to rows where the value in the ``id`` column is greater than 10 **and** the value in the ``name`` column starts with "a" or "A":

.. thumbnail:: ../../../img/graphql/manual/auth/composite-boolean-expression.png

.. Using roles
.. ###########


**Using session variables**
###########################

Session variable, that have been resolved from authentication tokens by either your authentication webhook or by Hasura using the JWT configuration, are available for constructing row-level permissions. 

E.g. to allow an ``author`` to access only their articles, you can use the ``X-Hasura-User-ID`` session variable to construct a rule to restrict access for ``select`` to rows in the ``articles`` table where the value in the ``id`` column is equal to the value in the session variable (*assuming this variable is being used to indicate the author's ID*):

.. thumbnail:: ../../../img/graphql/manual/auth/session-variables-in-permissions-simple-example.png

.. _relationships-in-permissions:

**Using relationships or nested objects**
#########################################

You can leverage relationships to define permission rules with fields from a nested object. Let's take the following example:

* An author/articles schema where an article can have one or more reviewers i.e. users with the role ``reviewer`` can only edit those articles that have been assigned to them:

.. thumbnail:: ../../../img/graphql/manual/auth/schema-for-nested-object-based-permissions.png

* The foreign key constraint from ``reviewers`` :: ``article_id``  →  ``articles`` :: ``id`` is used for an array relationship called  ``reviewers`` in the ``articles`` table:

.. thumbnail:: ../../../img/graphql/manual/auth/array-relationship-reviewers.png
   :class: no-shadow

We can use this relationship in a permission rule for the ``articles`` table  to limit access for users with the role ``reviewer`` to only assigned rows:

.. thumbnail:: ../../../img/graphql/manual/auth/nested-object-permissions-rule.gif

Via the relationship, we are using the ``reviewer_id`` field of the nested object ``reviewers`` in the the above permission rule that reads as "Allow updating an article if the **reviewer_id of any of the reviewers assigned to this article** is the same as the requesting user's id (*which is sent in the resolved session variable* ``X-Hasura-User-ID``)".

Let's say we have the following test data for the list of reviewers:

.. list-table:: Data in the ``reviewers`` table
   :header-rows: 1

   * - id
     - article_id
     - reviewer_id
   * - 1
     - 1
     - 5
   * - 2
     - 3
     - 5
   * - 3
     - 5
     - 5
   * - 4
     - 2
     - 6
   * - 5
     - 4
     - 6

Applying the above permission rule for "update" to "select" operation also, let's query the  ``articles`` table to watch this permission rule in action:

.. thumbnail:: ../../../img/graphql/manual/auth/restricted-data-for-role-reviewer.png
  :class: no-shadow

As we've made this query with the role ``reviewer`` and user ID ``5`` (*highlighted in the request headers in the above image*), we can only query those articles for which this user is a reviewer. This will be the case for update mutations too. As the user with id ``5`` does not have access to article with id ``2`` (*refer to the table above*), the following mutation will not update any rows of the ``articles`` table:

.. thumbnail:: ../../../img/graphql/manual/auth/unsuccessful-mutation-for-role-reviewer.png
  :class: no-shadow
   
.. admonition:: Array and Object relationships work similarly
  
  The above example would have worked even if the relationship were an object relationship. In our example, the corresponding rule for an object relationship would have read "*if this article's reviewer's id is the same as the requesting user's id, allow access to it*".

.. _col-level-permissions:

Column-level permissions
^^^^^^^^^^^^^^^^^^^^^^^^
Column-level permissions determine access to columns in the rows that accessible based on row-level permissions. These permissions are simple selections: 

.. thumbnail:: ../../../img/graphql/manual/auth/column-level-permissions.png

In this example, the role ``author`` has only partial access to columns of the accessible rows for the ``select`` operation.

.. _limit-rows-permissions:

Limiting number of rows
^^^^^^^^^^^^^^^^^^^^^^^

In the case of ``select`` operations, the number of rows to be returned in the response can be limited using this configuration:

.. thumbnail:: ../../../img/graphql/manual/auth/limit-rows-for-select.png

In the above example, this configuration  restricts the number of accessible rows (*based on the rule*: ``{"id":{"_eq":"X-Hasura-User-Id"}}``) to 20. 

.. _aggr-query-permissions:

Aggregation queries permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the case of ``select`` operations, access to :doc:`aggregation queries<../queries/aggregation-queries>` can be restricted for a given role using this configuration.

.. thumbnail:: ../../../img/graphql/manual/auth/aggregation-query-permissions.png

In the above example, the role ``user`` is allowed to make aggregation queries.

.. _col-presets-permissions:
Column presets
^^^^^^^^^^^^^^

While this is strictly not a permission configuration, defining :doc:`role-based column presets<../schema/default-values/column-presets>` on any column automatically removes access to it. This preset can be defined for ``insert`` and ``update`` operations. This configuration is also very useful to avoid sending sensitive user-information in the query and leverage session variables or static data instead.

``Select`` permissions
----------------------

For ``select`` operations or for GraphQL queries, you can configure the following:

* :ref:`Row-level permissions<row-level-permissions>`
   
* :ref:`Column-level permissions<col-level-permissions>`
   
* :ref:`Aggregration query permissions<aggr-query-permissions>`

* :ref:`Limiting number of rows<limit-rows-permissions>`


``Insert`` permissions
----------------------
For ``insert`` operations or for GraphQL mutations of the type *insert*, you can configure the following:

* :ref:`Row-level permissions<row-level-permissions>`
   
* :ref:`Column-level permissions<col-level-permissions>`

* :ref:`Column presets<col-presets-permissions>`

``Update`` permissions
----------------------
For ``update`` operations or for GraphQL mutations of the type *update*, you can configure the following:

* :ref:`Row-level permissions<row-level-permissions>`
   
* :ref:`Column-level permissions<col-level-permissions>`

* :ref:`Column presets<col-presets-permissions>`

``Delete`` permissions
----------------------
For ``delete`` operations or for GraphQL mutations of the type *delete*, you can configure the following:

* :ref:`Row-level permissions<row-level-permissions>`


