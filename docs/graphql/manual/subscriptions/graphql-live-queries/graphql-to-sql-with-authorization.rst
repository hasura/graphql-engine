Make authorization declarative
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Authorization when it comes to accessing data is essentially a constraint that depends on the values of data (or rows)
being fetched combined with application-user specific “session variables” that are provided dynamically.

To model this, we implemented an authorization layer similar to Postgres RLS at the API layer to provide a declarative
framework to configure access control. Now that authorization is declarative and available at a table, view or even a
function (if the function returns SETOF) it is possible to create a single SQL query that has the authorization rules.

GraphQL query → GraphQL AST → Internal AST with authorization rules → SQL AST → SQL

.. thumbnail:: ../../../../img/graphql/manual/subscriptions/graphql-2-sql-auth.png
  :alt: graphql to sql with authorization
