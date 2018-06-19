Upsert
======
To convert an *insert* mutation into an *upsert* one, you need to specify the unique constraint(s) and the action to be taken in the case of a conflict or violation. There are two ways to specify unique constraints, either specify the name of a unique constraint (using the ``constraint`` argument) or a list of columns that have unique constraints on them (using the ``constraint_on`` argument). On conflict, you can choose to either ignore the mutation (``action: "ignore"``) or update the row that caused the conflict (``action: "update"``).

For the following examples, assume there's a unique constraint on the ``name`` column of the ``author`` table.

.. note::
    
    You can fetch the name of unqiue constraints by quering the ``information_schema.table_constraints`` table. Typically, the constraint is automatically named as ``<table-name>_<column-name>_key`` when using the console to add it. The API-console will soon carry this information in the ``Data`` section.

With unique constraint name (update)
------------------------------------
Insert a new object in the author table or, if the unique constraint, ``author_name_key``, is violated, update the existing object:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint: "author_name_key",
                    action: "update"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 1
                }
            }
        }

The response shown above assumes that the name of the author in our object is not unique and then *updates* the corresponding row in the database.

With unique constraint name (ignore)
------------------------------------
Insert a new object into the author table or, if the unique constraint, ``author_name_key``, is violated, ignore the request:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint: "author_name_key",
                    action: "ignore"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 0
                }
            }
        }

In this case, the insert mutation is ignored because there is a conflict.

With columns having unique constraint (update)
----------------------------------------------
Insert a new object into the author table or, if a unique constraint on the specified columns, in this case - ``name``, is violated, update the existing object with values from the fields (in this case - ``id``):

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint_on: ["name"],
                    action: "update"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 1
                }
            }
        }

With columns having unique constraint (ignore)
----------------------------------------------
Insert a new object into the author table or, if a unique constraint on the specified columns, in this case - ``name``, is violated, ignore the request:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint_on: ["name"],
                    action: "ignore"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 0
                }
            }
        }

.. note::
    Primary key constraint is not the same as a unique constraint. So, if you include a column that is only part of a primary key as one of the ``constraint_on`` argument's parameters, you will run into the following error: ``there is no unique or exclusion constraint on target column(s)``.