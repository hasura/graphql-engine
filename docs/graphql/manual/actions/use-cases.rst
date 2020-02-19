Actions sample use cases
========================


.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


WORK IN PROGRESS

Actions are ideal for doing custom business logic including data validation, etc.


Data validation
---------------

Suppose you want to insert an article only if the article's character length is greater than 500 and the author has less than 10 articles.

WORK IN PROGRESS

Complex form data
-----------------

When you have to take input in a custom structure or your table models are not best suited for input forms.

WORK IN PROGRESS

Data enrichment
---------------

After performing some custom logic, you may need to return more data to the front-end client. You can do this by creating relationships between actions and your tables.

WORK IN PROGRESS

Custom auth
-----------

Suppose you have an existing auth system which is hard to map to Hasura's permission system. Then, you can allow only actions to mutate data and perform custom auth in the action handler.

WORK IN PROGRESS
