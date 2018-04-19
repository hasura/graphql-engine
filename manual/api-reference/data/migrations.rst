.. .. meta::
   :description: Learn how to manage the schema and metadata of Hasura's Postgres instance in a project and how to migrate it from one project to another.
   :keywords: hasura, docs, data, schema migration, data migration, metadata migration, manage schema


Managing schema and metadata
============================

.. todo::

   Needs to be severely updated


Using the console to create tables, relationships, permissions etc. helps us to quickly iterate in the early stages of the project. The major downside of this approach is that once we have multiple environments (staging, dev etc), it'll be hard to replicate the changes across all these instances. This is even critical once the project is in production. Every change has to be carefully planned, tested and deployed.

The best practice to manage schema of a relational database is to use migrations. You can choose any tool that you are comfortable with. We use alembic at Hasura for managing schema in our own projects.
The recommended approach to manage the ``data`` microservice is to have a single file that describes the relationships, permissions and query templates across all tables. Since this metadata (relationships/permissions etc) of the data microservice is dependent on the schema, we need to keep the metadata in sync with the schema. This is the workflow that helps in doing this:

Check the migrations into version control. The ``data`` microservice's metadata file should also be checked into version control. So, when we add a new migration that would affect the ``data`` microservice's metadata, we edit the metadata file too (if needed).

To apply these changes:

1. Run schema migrations.
2. Apply the modified metadata.

Once both the steps are successful, we can commit the changes into version control.

While that is the core idea, we may have to tweak it a bit depending on the scenario:

Starting from scratch
---------------------

This is the case where the console is never used to manage the project from the very beginning. We start with empty schema and empty metadata file. Every change to Postgres schema and data microservice is done by migrations or metadata file. This would be the ideal scenario.

From console to migrations
--------------------------

This is the most typical scenario. We have used the console to setup schema and metadata. Now, we would like to move to using migrations and metadata file. The steps are as follows:

1. Use pg_dump to take a schema only dump of the ``hasuradb`` database for all the relevant schemas (typically only 'public'). Let's call this the baseline schema.
2. Export the current metadata from the running ``data`` microservice.
3. Check these into source control.
4. Initialise migrations. Create a new migration which will add the baseline schema.
5. Run the migration and apply the metadata.

Setting up a new instance of the project
----------------------------------------

Please setup the migrations and metadata for your project if you haven't already set it up.

Point the migration context to the new instances's postgres, run the migrations and the apply the metadata.

Applying changes to different instances
---------------------------------------

If you have more than one environment (instance) of your project, to propagate the changes, simply change the migration context to the appropriate postgres instance, run the migrations and apply the metadata.

.. _Metadata:

Metadata file syntax
--------------------

The syntax of the metadata file is as follows

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - tables
     - true
     - TableMetadata_ array
     - Metadata on all tables
   * - query_templates
     - true
     - :ref:`QueryTemplateMetadata <create_query_template_syntax>` array
     - All query templates

TableMetadata
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - object_relationships
     - false
     - ObjRelMetadata_ array
     - Object relationships on the table
   * - array_relationships
     - false
     - ArrRelMetadata_ array
     - Array relationships on the table
   * - insert_permissions
     - false
     - InsPermMetadata_ array
     - Insert permissions on the table
   * - select_permissions
     - false
     - SelPermMetadata_ array
     - Select permissions on the table
   * - update_permissions
     - false
     - UpdPermMetadata_ array
     - Update permissions on the table
   * - delete_permissions
     - false
     - DelPermMetadata_ array
     - Delete permissions on the table

ObjRelMetadata
&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the relationship
   * - using
     - true
     - :ref:`ObjRelUsing <ObjRelUsing>`
     - Use one of the available ways to define array relationship
   * - comment
     - false
     - text
     - comment

ArrRelMetadata
&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the relationship
   * - using
     - true
     - :ref:`ArrRelUsing <ArrRelUsing>`
     - Use one of the available ways to define array relationship
   * - comment
     - false
     - text
     - comment

InsPermMetadata
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - :ref:`InsertPermission <InsertPermission>`
     - The permission definition
   * - comment
     - false
     - text
     - comment

SelPermMetadata
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - :ref:`SelectPermission <SelectPermission>`
     - The permission definition
   * - comment
     - false
     - text
     - comment

UpdPermMetadata
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - :ref:`UpdatePermission <UpdatePermission>`
     - The permission definition
   * - comment
     - false
     - text
     - comment

DelPermMetadata
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - :ref:`DeletePermission <DeletePermission>`
     - The permission definition
   * - comment
     - false
     - text
     - comment

Metadata related queries
------------------------

These operate at the entire metadata level. The following are the queries:

clean_metadata
^^^^^^^^^^^^^^

This query clears the entire metadata of the data microservice. This can be used when you want to start with a clean slate.

.. code-block:: http
   :emphasize-lines: 6

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

   {
       "type": "clean_metadata",
       "args": {}
   }

.. _CleanMetadata:

Syntax
&&&&&&

It currently takes no arguments. Just an empty object.

export_metadata
^^^^^^^^^^^^^^^

This query exports the entire metadata of the data microservice.

.. code-block:: http
   :emphasize-lines: 6

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

   {
       "type": "export_metadata",
       "args": {}
   }

The response would be the same format as the metadata file.

.. _ExportMetadata:

Syntax
&&&&&&

It currently takes no arguments. Just an empty object.

set_metadata
^^^^^^^^^^^^

This query lets you set the metadata of the ``data`` microservice. Note that the old metadata will be replaced with the one given in the query.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

   {
       "type": "set_metadata",
       "args": {
           "tables" : [
               {
                   "table" : "article",
                   "object_relationships" : [
                       {
                           "name" : "author",
                           "using" : {
                               "foreign_key_constraint_on" : "author_id"
                           }
                       }
                   ]
               },

               {
                   "table" : "author",
                   "array_relationships" : [
                       {
                           "name" : "articles",
                           "using" : {
                               "foreign_key_constraint_on" : {
                                   "table" : "article",
                                   "column" : "author_id"
                               }
                           }
                       }
                   ]
               }
           ]
       }
   }

.. _SetMetadata:

Syntax
&&&&&&

:ref:`Metadata <Metadata>`
