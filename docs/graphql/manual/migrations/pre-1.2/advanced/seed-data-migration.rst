.. meta::
   :description: Create a seed data migration in Hasura
   :keywords: hasura, docs, migration, seed data

.. _seed_data_migration_old:

Creating a seed data migration (pre v1.2)
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

It can be convenient to add data into tables as part of the DB init process. This section gives instructions as to how to achieve that.

Step 1: Run the console via the Hasura CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to make sure that the migrations get created, the console needs to be run via the Hasura CLI.

Step 2: Navigate to the SQL section
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the Hasura console, click on the ``Data`` tab and then on the ``SQL`` link on the left hand side.


Step 3: Write an insert statement
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The next step is to write an insert statement that populates the database with seed data, like this:

.. code-block:: SQL

    INSERT INTO addresses (id, location) VALUES
      (1, 'Bangalore'),
      (2, 'Tel Aviv'),
      (3, 'Zurich');
    INSERT INTO authors (id, name, address_id) VALUES
      (1, 'Sarah', 3),
      (2, 'Joey', 1),
      (3, 'Rachel', 2);
    INSERT INTO articles (id, title, content, author_id) VALUES
      (1, 'How to make fajitas', 'Recipe on making the best fajitas in the world', 1),
      (2, 'How to climb mount everest', 'Guide on successfully climbing the hightest mountain in the world', 3),
      (3, 'How to be successful on broadway', 'What it takes for you to be a successful performer at broadway', 2);

Step 4: Mark the insert as a migration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check the box ``This is a migration`` and give the migration a name, e.g. ``insert_seed_data``.

Step 5: Run the statement
^^^^^^^^^^^^^^^^^^^^^^^^^

Hit the ``Run!`` button. 

Step 6: Verify data & migration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the insert statement was successful, the data is now added to the DB. 

Navigate to the ``migrations`` directory in your Hasura project. The latest migration
will be the the insert statement that was just run.
