.. meta::
   :description: Updating v1 Cloud project to v2
   :keywords: hasura, docs, project, upgrade, update, v2

.. _update_project_v2:

Updating Hasura Cloud v1.3 projects to Hasura v2.0
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura Cloud now creates new projects with Hasura ``v2.0`` by default. Due to
some behaviour and underlying architectural changes in ``v2.0``, existing
projects have not been auto-updated to ``v2.0``. You can update your older ``v1.3``
projects to ``v2.0`` by following this guide.

(*You can find the older guide to do this process manually* :ref:`here <move_project_v2>`)

Pre-update steps
----------------

Check the :ref:`core updating to v2.0 <upgrade_hasura_v2>` guide and the
`release notes <https://github.com/hasura/graphql-engine/releases>`__ to see what new concepts,
features and behaviour changes have been introduced in Hasura ``v2.0``.

Estimated time needed for update
--------------------------------

Depending on the size of your project the update process should typically take around **5-10 minutes** to be completed.

Project availability during update
----------------------------------

During the update process Hasura Cloud will need to place your project in a "Maintenance mode" till the process is complete.

When **Cloud maintenance mode** is enabled, all Hasura Cloud actions such as updating env vars, transferring ownership, etc. will be disabled.

When **Server maintenance mode** is enabled, all actions updating the project metadata such as tracking tables, adding relationships, etc.
will be disabled.

Hence it is recommended to update your project at a time convenient to all project collaborators.

Apart from the brief moments needed to enable/disable the server maintenance mode, **your GraphQL API will continue to function during this period.**

Post-update steps
-----------------

- As with most major updates, we would recommend you to monitor and ensure all functionalities of your project
  are working as expected post the update and the update did not cause any unexpected changes. Do get in touch
  with us in case you notice anything unexpected.

- You can choose to remove the following env vars that were added to your project during the update
  to preserve some ``v1.3`` behaviours that were modified in ``v2.0``. We recommend moving to the new behaviour
  if your project is not impacted by the changes.

  - ``HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE``

  Check the :ref:`Behaviour changes <hasura_v2_behaviour_changes>` section of the core updating to v2.0 guide
  for details regarding the behaviour changes.

- Check the :ref:`Post update steps <hasura_v1_to_v2_post_update_steps>` section of the core updating to v2.0 guide
  for other changes you might wish to make post your project update.

Update process
--------------

.. admonition:: Update option might not be currently enabled for all users

  If you do not see the option to update your project as mentioned below yet, you can expect to see so
  in the coming few days.

To start the update, you should see a button called ``Update`` next to your project on the Project list page
as shown below:

.. thumbnail:: /img/graphql/cloud/projects/update-project.png
   :width: 700px
   :alt: Update project

Clicking on this button will trigger an update job that will perform a few tasks to update your current
``v1.3`` project to ``v2.0``.

Refer to the following task wise breakdown of the update job to understand what the job will be doing
and to check your project end state in case of any failures.

Do reach out to support if you observe any issues with the update process or run into any other problems
post the v2.0 update.

Update job sub-tasks
^^^^^^^^^^^^^^^^^^^^

The following is a task wise breakdown of the update to v2.0 job.

Each task in the update job has a **rollback strategy** in case any failures occur. If the rollback steps of any task
fail, the project might be in an unhealthy state in which case please get in touch with support immediately
for assistance.

.. contents::
  :backlinks: none
  :depth: 2
  :local:

Step 1: Initializing
""""""""""""""""""""

Step 1.1: Validating
********************

Sub-tasks:
~~~~~~~~~~

- Enable cloud maintenance mode
- Ensure infrastructure for update is available

On Failure:
~~~~~~~~~~~

- Disable cloud maintenance mode
- **Project stays in v1.3**

Step 1.2: Enabling maintenance mode
***********************************

Sub-tasks:
~~~~~~~~~~

- Set ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to enable server maintenance mode
- Set ``HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE`` env var to ``true`` to maintain
  ``v1.3`` behaviour for ``null`` values in ``where`` filters. :ref:`(Know more) <hasura_v2_behaviour_changes>`

On Failure:
~~~~~~~~~~~

- Unset ``HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE`` env var
- Unset ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to disable server maintenance mode
- Disable cloud maintenance mode
- **Project stays in v1.3**

Step 2: Updating project
""""""""""""""""""""""""

Step 2.1: Migrating project metadata
************************************

Sub-tasks:
~~~~~~~~~~

- Take a backup of server metadata from user database & move metadata to cloud metadata database

On Failure:
~~~~~~~~~~~

- Unset ``HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE`` env var
- Unset ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to disable server maintenance mode
- Disable cloud maintenance mode
- **Project stays in v1.3**

Step 2.2: Creating v2.0 instance
********************************

Sub-tasks:
~~~~~~~~~~

- Create ``v2.0`` instance
- Start routing requests to ``v2.0`` instance

On Failure:
~~~~~~~~~~~

- Start routing requests back to ``v1.3`` instance
- Unset ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to disable server maintenance mode
- Disable cloud maintenance mode
- **Project stays in v1.3**

Step 2.3: Migrating pending events, async actions data
******************************************************

Sub-tasks:
~~~~~~~~~~

- Send signal to shutdown ``v1.3`` instance
- Wait for ``v1.3`` to gracefully shutdown after completing processing of any in-flight events
- Migrate pending events, async actions data to cloud metadata database

On Failure:
~~~~~~~~~~~

- Restart ``v1.3`` instance
- Start routing requests back to ``v1.3`` instance
- Unset ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to disable server maintenance mode
- Disable cloud maintenance mode
- **Project stays in v1.3**

.. _v2_update_migrate_invocation_logs:

Step 2.4: Migrating processed events, async actions data
********************************************************

Sub-tasks:
~~~~~~~~~~

- Migrate invocation logs of processed events, async actions to cloud metadata database

On Failure:
~~~~~~~~~~~

- **No action taken. Job continues to next task**
- Invocation logs of already processed events, async actions are not migrated. Contact support
  to assist with a manual migration of the logs if needed

Step 3: Validating update
"""""""""""""""""""""""""

.. _v2_update_disable_maintenance_mode:

Step 3.1: Disabling maintenance mode
************************************

Sub-tasks:
~~~~~~~~~~

- Unset ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE`` env var to disable server maintenance mode
- Disable cloud maintenance mode

On Failure:
~~~~~~~~~~~

- **No action taken. Job continues to next task**
- Server maintenance mode can be disabled manually by setting ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE``
  env var to ``false``
- Contact support if your project is in an unexpected state

.. _v2_update_check_consistency:

Step 3.2: Check metadata consistency
************************************

Sub-tasks:
~~~~~~~~~~

- Check if metadata is consistent

On Failure:
~~~~~~~~~~~

- **No action taken. Job continues to next task**
- Check your project metadata status on the console *(Settings (⚙) -> Metadata status)* and attempt reloading metadata
  if there is an unexpected inconsistency reported. Contact support if the inconsistency doesn't go away on
  metadata reload.

4. Project update complete
""""""""""""""""""""""""""

Project update to ``v2.0`` is completed.
