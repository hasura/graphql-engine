Access control examples
=======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This is a guide to help you set up a basic authorization architecture for your GraphQL fields. It is recommended
that you first check out :doc:`roles-variables` and :doc:`permission-rules`
that will be referred to throughout this guide.

Here are some examples of common use-cases.

Anonymous (not logged in) users
-------------------------------

- Create a role called ``anonymous`` (this value is up to you, you could even name the role ``public``).
- Generally, you wouldn't add insert, update, or delete permissions.
- For the select permission condition, create a valid condition depending on your data model. For example,
  ``is_published: {_eq: true}``.
- If you don't have a condition, then just set the permission to ``Without any checks``, represented by a ``{}``.
- Choose the right set of columns that will get exposed in the GraphQL schema as fields. Ensure that sensitive
  information will not be exposed.

.. thumbnail:: ../../../../img/graphql/manual/auth/anonymous-role-examples.png
   :class: no-shadow

You can use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or ``--unauthorized-role`` flag to set a role
for non-logged in users. The configured unauthorized role will be used whenever an access token is not present
in a request to the GraphQL API.

Logged-in users
---------------

- Create a role called ``user``.
- Access control rules in this case are usually dependent on a ``user_id`` or a ``owner_id`` column in your data model.
- Set up a permission for insert/select/update/delete that uses said column. E.g.:
  ``author_id: {_eq: "X-Hasura-User-Id"}`` for an article table.
- Note that the ``X-Hasura-User-Id`` is a :doc:`dynamic session variable<./roles-variables>` that comes in from
  your :doc:`auth webhook's <../authentication/webhook>` response, or as a request as a header if you're testing.

.. thumbnail:: ../../../../img/graphql/manual/auth/user-select-graphiql.png
   :class: no-shadow


Managers of an organisation in a multi-tenant app
-------------------------------------------------

Suppose you have a multi-tenant application where managers of a particular organisation can see all of the data that
belongs to the organisation. In this case, your data models will probably have an ``org_id`` column that denotes the
organisation either in the same table or via a related table.

- Create a role called ``manager``.
- Create a permission for select, which has the condition: ``org_id: {_eq: "X-Hasura-Org-Id"}``.
- ``X-Hasura-Org-Id`` is a :doc:`dynamic variable <./roles-variables>` that is returned by your
  :doc:`auth webhook <../authentication/webhook>` for an incoming GraphQL request.

.. thumbnail:: ../../../../img/graphql/manual/auth/org-manager-graphiql.png
   :class: no-shadow

Collaborators of an article
---------------------------

Let's say the "ownership" or "visibility" information for a data model (table) is not present as a column in the table, but in a different related table. In this case, let's say there is an ``article`` table and a ``collaborator`` table that has ``article_id, collaborator_id`` columns.

- Create a relationship called ``collaborators`` from the article table.

  - Array relationship (article has array of collaborators): ``article :: id → collaborator :: article_id``.

- Create a role called ``collaborator``.
- Create a select permission on the ``article`` table, which has the condition:
  ``collaborators: {collaborator_id: {_eq: "X-Hasura-User_id"}}``.

  - This reads as: Allow the role collaborator to select if ``article.collaborators`` has a ``collaborator_id``
    equal to that of ``X-Hasura-User-Id``.

.. thumbnail:: ../../../../img/graphql/manual/auth/collaborator-relationship.png
   :class: no-shadow


.. Role-based schemas
  ------------------

  For every role that you create, Hasura automatically publishes a different GraphQL schema that represents the
  right queries, fields, and mutations that are available to that role.

  Case 1: Logged-in users and anonymous users can access the same GraphQL fields
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  In simple use-cases, logged-in users and anonymous users might be able to fetch different rows (let's say because
  of a ``is_public`` flag), but have access to the same fields.

  - ``anonymous`` role has a ``{is_public: {_eq: true}}`` select condition.

    - This reads: Allow anyone to access rows that are marked public.

  - ``user`` role has a ``_or: [{is_public: {_eq: true}}, {owner_id: {_eq: "X-Hasura-User-Id"}}]``.

    - This reads: Allow users to access any rows that are public, or that are owned by them.

  Case 2: Logged-in users and anonymous users have access to different fields
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  In this case, anonymous users might have access only to a subset of fields while logged-in users can access all the
  fields for data that they own.

  - ``anonymous`` role has a ``{is_public: {_eq: true}}`` select condition, and only the right columns are allowed to
    be selected.

    - This reads: Allow anyone to access rows that are marked public.

  - ``user`` role has a ``{owner_id: {_eq: "X-Hasura-User-Id"}}`` and all the columns are marked as selected.

    - This reads: Allow users to that are owned by them.


.. _nested-object-permissions-example:

Multiple roles per user
-----------------------

Sometimes your data/user model requires that:

* Users can have multiple roles.
* Each role has access to different parts of your database schema. 

If you have the information about roles and how they map to your data in the same database as the one configured with GraphQL Engine, you can leverage relationships to define permissions that effectively control access to data and the operations each role is allowed to perform. 

To understand how this works, let's model the roles and corresponding permissions in the context of a blog app wth the following roles:

* ``author``: Users with this role can submit **their own** articles. 

* ``reviewer``: Users with this role can review **articles assigned to them** and add a review comment to each article. A mapping of articles to reviewers is maintained in the ``reviewers`` table.  

* ``editor``: Users with this role can edit and publish **any article**. They can also leave a private rating for each article. However, they cannot overwrite a reviewer's notes. A list of editors is maintained in the ``editors`` table.

Database Schema
^^^^^^^^^^^^^^^

The following is a reference database schema for our example:

.. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-db-schema.png

Based on this schema, we'll create the following tables:

.. code-block:: sql

  -- user information from your auth system

  users (
    id INT PRIMARY KEY,
    name TEXT,
    profile JSONB, -- some profile information like display_name, etc.
    registered_at TIMESTAMP -- the time when this user registered 
  )
  
  -- information about articles

  articles (
    id INTEGER PRIMARY KEY,
    title TEXT,
    author_id INT REFERENCES users(id), -- Foreign key to users :: id
    is_reviewed BOOLEAN DEFAULT FALSE,
    review_comment TEXT,
    is_published BOOLEAN DEFAULT FALSE,
    editor_rating INTEGER
  )

  -- mapping of reviewers to articles
  
  reviewers (
    id INTEGER PRIMARY KEY,
    article_id INTEGER REFERENCES articles(id), -- Foreign key to articles :: id
    reviewer_id INTEGER REFERENCES users(id) -- Foreign key to users :: id
  )

  -- a  list of editors

  editors (
    editor_id INTEGER PRIMARY KEY REFERENCES users(id) -- Foreign key to users :: id
  )

Relationships
^^^^^^^^^^^^^

Create an array relationship named ``reviewers`` based on the foreign key constraint ``reviewers`` :: ``article_id``  →  ``articles`` :: ``id``:

.. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-reviewers-array-relationship.png
     :class: no-shadow

Permissions
^^^^^^^^^^^
The following is an example summary of the access control requirements for the ``articles`` table based on the above schema:

+----------------+-------------+--------+--------+--------+--------+--------+
+ Columns of     | author               | reviewer        | editor          |
+ the ``article``+-------------+--------+--------+--------+--------+--------+
| table          | insert      | select | update | select | update | select |
+================+=============+========+========+========+========+========+
| id             | ✔️          | ✔️     | ❌     | ✔️     | ❌     | ✔️     |
+----------------+-------------+--------+--------+--------+--------+--------+
| title          | ✔️          | ✔️     | ✔️     | ✔️     | ✔️     | ✔️     |
+----------------+-------------+--------+--------+--------+--------+--------+
| author_id      | ✔️ :sup:`*` | ✔️     | ❌     | ✔️     | ❌     | ✔️     | 
+----------------+-------------+--------+--------+--------+--------+--------+
| is_reviewed    | ❌          | ✔️     | ✔️     | ✔️     | ✔️     | ✔️     | 
+----------------+-------------+--------+--------+--------+--------+--------+
| review_comment | ❌          | ✔️     | ✔️     | ✔️     | ❌     | ✔️     | 
+----------------+-------------+--------+--------+--------+--------+--------+
| is_published   | ❌          | ✔️     | ❌     | ✔️     | ✔️     | ✔️     |
+----------------+-------------+--------+--------+--------+--------+--------+
|editor_rating   | ❌          | ❌     | ❌     | ❌     | ✔️     | ✔️     |
+----------------+-------------+--------+--------+--------+--------+--------+

:sup:`*` *Additional restriction required to ensure that a user with the role* ``author`` *can submit only their own article i.e.* ``author_id`` *should be the same as the user's id*.


We'll create permission rules for the roles and actions listed above (*you can easily extend them for the actions not documented here*) .

Permissions for role ``author``
"""""""""""""""""""""""""""""""

* **Allow users with the role** ``author`` **to insert only their own articles**
  
  For this permission rule, we'll make use of two features of the GraphQL Engine's permissions system:

  a) :ref:`Column-level permissions<col-level-permissions>`: Restrict access to certain columns only.
  
  b) :doc:`Column presets <../../schema/default-values/column-presets>`: Session-variable-based column preset for the ``author_id`` column to automatically insert the user's ID i.e. the ``X-Hasura-User-Id`` session-variable's value. It also helps us avoid explicitly passing the user's ID in the insert mutation.


  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-author-insert.png

  Notice how we don't need to have an explicit row-level permission (*a custom check*) as only authenticated users with the role ``author`` can perform this action. As we have a column preset for the ``author_id`` column that automatically takes the author's ID (*and the* ``id`` *column is an auto-increment integer field*), we only need to allow access to the ``title`` column.

* **Allow users with the role** ``author`` **to select certain columns only**

  Again, we'll  use **column-level** permissions to restrict access to certain columns. Additionally, we need to define row-level permissions (*a custom check*) to restrict access to only those articles authored by the current user:

  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-author-select.png
     
  
  The row-level permission rule shown here translates to "*if the value in the* ``author_id`` *column of this row is equal to the user's ID i.e. the* ``X-Hasura-User-Id`` *session-variable's value, allow access to it*". 

Permissions for role ``reviewer``
"""""""""""""""""""""""""""""""""

* **Allow users with the role** ``reviewer`` **to update articles assigned to them for reviews**
  
  For this use-case, we'll use :ref:`relationship or nested-object permissions<relationships-in-permissions>` based on the array relationship ``reviewers`` to restrict access to assigned articles only.

  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-reviewer-update.png

  The array-relationship based permission rule in the above image reads as "*if the ID of any of reviewers assigned to this article is equal to the user's ID i.e. the* ``X-Hasura-User-Id`` *session-variable's value, allow access to it*". The columns' access is restricted using the column-level permissions highlighted above.

* **Allow users with the role** ``reviewer`` **to select articles assigned to them for reviews**

  This permission rule is pretty much the same as the one for update, the only difference being the  column-level permissions.  

  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-reviewer-select.png

Permissions for role ``editor``
"""""""""""""""""""""""""""""""

* **Allow editors to select any article's data**

  This is a straightforward rule - there's no need for any row-level permissions since editors have access to all rows and they can *read* all columns.

  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-editor-select.png
  
* **Allow editors to update an article**

  There's no need for row-level permissions in this case either but we need to restrict access to certain columns only:

  .. thumbnail:: ../../../../img/graphql/manual/auth/multirole-example-editor-update.png



























