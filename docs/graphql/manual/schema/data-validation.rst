.. meta::
   :description: Data validation in Hasura
   :keywords: hasura, docs, schema, data validation

.. _data_validation:

Data validation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Sometimes, we need to perform some kind of validation in the context of a database event like ``insert``, ``update`` or ``delete``.

For example, in an online shop, we need the following types of validation (among others):

- Before a user can add an item to a cart, we need to make sure that we have the respective amount of items in stock.
- During checkout, we need to make sure that the user is logged in with a valid token

The best solution to implement validation depends on the complexity of the use case. 

Simple checks
-------------

Simple checks can be done using a
`Postgres trigger <https://www.postgresql.org/docs/9.1/sql-createtrigger.html>`__.

To make sure a user can only add as many items to the cart as we have in stock, we can write the following Postgres function:

## Postgres function

Now let's create a trigger that calls this function before we can add something to the cart.

## Postgres trigger

If we now try to add something to the cart where we don't have enough in stock, the following error will show:

## Error

Complex data validation
-----------------------

For more complex validation use cases like user login, we recommend using :ref:`Hasura actions <actions>`. 

An example of an action handler implementation for user login can be found :ref:`here <action_handlers>`.
