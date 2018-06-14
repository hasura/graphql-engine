Mutations
=========

Bulk mutations
--------------

Multiple queries/mutations in single call


Insert
------

Possible to insert multiple objects at a time

Upsert
------

Handle insert if object already present ie: conflict occurs

- Choose constraint which can cause conflict
- On conflict

  - Update
  - Ignore

Update
------

Update objects based on filter on own fields or nested objects' fields

- $set - set values of fields
- $inc - increment values of fields by given value
- $mul - multiply values of fields by given value
- $default - set values of fields to their default value


Delete
------

Delete objects based on filter on own fields or nested objects' fields


