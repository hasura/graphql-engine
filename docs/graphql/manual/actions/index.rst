Actions
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


WORK IN PROGRESS

Actions are user defined mutations with custom business logic. Actions can be added to Hasura to handle various use cases such as data validation, data enrichment and other complex business logic.

When the permissions system isn't enough to specify the required constraints, you would typically add such mutation through a remote schema. However actions can handle these use cases better because of the following reasons:

1. No need to write a remote schema. Actions can be executed in ordinary webhooks or postgres itself.

2. Return graphql-engine's types without writing any extra code. You might want to return the new "state" after a mutation.

3. Can be executed asynchronously for building powerful event-driven apps.

Architecture Diagram
--------------------

WORK IN PROGRESS


Learn more
----------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Getting started <getting-started>
  Input types <input-types>
  Response types <response-types>
  Action handlers <action-handlers>
  Async actions <async-actions>
  Sample use cases <use-cases>
