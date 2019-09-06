Allow-list for queries
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

**Allow-list** is a list of safe queries (*GraphQL queries, mutations or subscriptions*) that is stored by
GraphQL engine in its metadata. When enabled, it can be used to restrict GraphQL engine so that it
executes **only** those queries that are present in the list *(available after version v1.0.0-beta.1)*.

Adding or removing a query in allow-list
----------------------------------------

You can add or remove a query in the allow-list in two ways:

* **Using the console:**  Head to the ``Settings`` (⚙) --> ``Allowed queries`` section in the console. You can
  add a new query to the allow-list or upload a list of new queries from a file that will be added to the
  allow-list. You can also see a list of existing queries in the allow-list and delete them individually.

  * You can add an individual query, like the one below, manually to the allow-list with a unique name.

    .. code-block:: graphql

     query ($id: Int!){
        user_by_pk(id: $id){
          __typename
          id
          name
          company
        }
     }

  * You can upload files, like this `sample file <https://gist.github.com/dsandip/8b1b4aa87708289d4c9f8fd9621eb025>`_,
    to add multiple queries to the allow-list (each query needs to have a name).

* **Using metadata APIs:** Queries can be stored in collections and a collection(s) can added to or removed
  from the allow-list. See :doc:`Collections & Allow-list APIs<../api-reference/schema-metadata-api/query-collections>`
  for API reference.

.. note::

  * ``__typename`` introspection fields will be ignored when adding queries and comparing them to the allow-list.

  * Any introspection queries that your client apps require will have to be explicitly added to the allow-list
    to allow running them.

  * The order of fields in a query will be **strictly** compared. E.g. assuming the query in first example
    above is part of the allow-list, the following query will be **rejected**:

    .. code-block:: graphql

     query ($id: Int!){
        user_by_pk(id: $id){
          __typename
          name
          id
          company
        }
     }

  * Allow-list is stored in the metadata. To version control the state of the list, you are required to export
    the metadata. See :doc:`Managing Hasura metadata <../migrations/manage-metadata>` for more details.

  * You can modify the allow-list without actually enabling it on your instance.


Enable allow-list
-----------------

The allow-list validation can be enabled by setting the ``HASURA_GRAPHQL_ENABLE_ALLOWLIST`` environment
variable to ``true`` or running GraphQL engine with the ``--enable-allowlist`` flag (*default value is*
``false``). See  :ref:`reference docs <command-flags>`.

.. note::

  The allow-list validation will not be enforced for the ``admin`` role.

Recommended usage
-----------------

The following are the recommended best practises for enabling/disabling allow-list  based validation:

* **In development instances**: During development or in dev instances, disable allow-list (*default setting*)
  to allow complete access to the GraphQL schema. Add/remove queries in the allow-list and then export the
  metadata for version-control (*so you can apply to it other instances*).

* **In CI/CD instances**: Enable allow-list for testing. 

* **In production instances**: Enabling allow-list is highly recommended when running GraphQL engine in production. 



