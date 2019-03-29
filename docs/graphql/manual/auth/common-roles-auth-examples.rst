Access control examples
=======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This is a guide to help you set up a basic authorization architecture for your GraphQL fields.

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

.. thumbnail:: ../../../img/graphql/manual/auth/anonymous-role-examples.png
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
  your :doc:`auth webhook's<./webhook>` response, or as a request as a header if you're testing.

.. thumbnail:: ../../../img/graphql/manual/auth/user-select-graphiql.png
   :class: no-shadow


Managers of an organisation in a multi-tenant app
-------------------------------------------------

Suppose you have a multi-tenant application where managers of a particular organisation can see all of the data that
belongs to the organisation. In this case, your data models will probably have an ``org_id`` column that denotes the
organisation either in the same table or via a related table.

- Create a role called ``manager``.
- Create a permission for select, which has the condition: ``org_id: {_eq: "X-Hasura-Org-Id"}``.
- ``X-Hasura-Org-Id`` is a :doc:`dynamic variable<./roles-variables>` that is returned by your
  :doc:`auth webhook <./webhook>` for an incoming GraphQL request.

.. thumbnail:: ../../../img/graphql/manual/auth/org-manager-graphiql.png
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

.. thumbnail:: ../../../img/graphql/manual/auth/collaborator-relationship.png
   :class: no-shadow

Role-based schemas
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


Multiple roles per user
-----------------------

Sometimes your data/user model requires that users be assigned multiple roles, and each role have access to different parts of your database schema. If you have the information about roles and how they map to your data in the same database as the one configured with GraphQL Engine, you can leverage relationships to define permissions that effectively control access to data and the operations each role is allowed to perform. 

To understand how this works, let's model the roles and corresponding permissions in the context of a blog app wth the following roles:

* ``author``: Users with this role can submit **their own** articles. 

* ``reviewer``: Users with this role can review **articles assigned to them** and add a review comment to each article. A mapping of articles to reviewers is maintained in the ``reviewers`` table.  

* ``editor``: Users with this role can edit and publish **any article**. They can also leave a private rating for each article. However, they cannot overwrite a reviewer's notes. A list of editors is maintained in the ``editors`` table.

.. thumbnail:: ../../../img/graphql/manual/auth/multirole-setup.png
   :class: no-shadow


Database Schema
^^^^^^^^^^^^^^^

We'll create the following tables:

.. code-block:: sql

  -- user information from your auth system

  users (
    id INT PRIMARY KEY,
    name TEXT,
    profile JSONB, -- some profile information like display_name, etc.
    registered_at timestampz -- the time when this user registered 
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

Create an array relationship ``reviewers`` based on the foreign key constraint ``article_reviewer`` :: ``article_id``  →  ``articles`` :: ``id``:

.. thumbnail:: ../../../img/graphql/manual/auth/reviewers-array-relationship.png
     :class: no-shadow

Permissions
^^^^^^^^^^^

We'll now create permission rules to restrict access to rows and columns of the ``articles`` table as per our requirements:

* **Allow users with the role** ``author`` **to insert/select/update/delete only their own articles**
  
  For insert permission, we need to just configure a session-variable-based :doc:`column preset <../schema/default-values/column-presets>` for the ``author_id`` column as this will automatically accept only the user's ID i.e. the ``X-Hasura-User-Id`` header's value. It also helps us avoid explicitly passing the user's ID in the mutation.

  .. thumbnail:: ../../../img/graphql/manual/auth/author-insert-preset.png
     :class: no-shadow

  For update, select and delete permissions, we need to configure the following rule:

  .. thumbnail:: ../../../img/graphql/manual/auth/author-update-permissions.png
     :class: no-shadow
  
  The update permission rule shown here translates to "*if the value in the* ``author_id`` *column of this row is equal to the user's ID i.e. the* ``X-Hasura-User-Id`` *header's value, allow access to it*". We'll also remove access to the ``author_id`` column in the column permissions so it cannot be modified.
  
  The same rule can be used for select and delete too, but the column permission shown above is not required for select (*and is not applicable to delete*).

  If we now query the ``articles`` table as an ``author`` i.e with the header ``X-Hasura-Role`` set to ``author`` and ``X-Hasura-User-ID`` set to a user's ID, we will only see articles written by this author in the response:

  .. thumbnail:: ../../../img/graphql/manual/auth/restricted-data-for-author-role.png
     :class: no-shadow

* **Allow users with the role** ``reviewer`` **to only view articles assigned to them for reviews and edit only the content of such articles**

  .. thumbnail:: ../../../img/graphql/manual/auth/reviewer-select-permissions.png
     :class: no-shadow

  The array-relationship based permission rule in the above image reads as "*if the ID of any of reviewers assigned to this article is equal to the user's ID i.e. the* ``X-Hasura-User-Id`` *header's value, allow access to it*".

  The same rule can be applied to both update and delete, and in the case of update, we'll use column permissions, as shown above in the update permission for the role ``author``, to restrict access to only the ``title`` column, so that only the content can be updated by a reviewer.

  In the previous example query, if we modify the role from ``author`` to ``reviewer``, we will see only those articles that have been assigned to this user for a review :
  
  .. thumbnail:: ../../../img/graphql/manual/auth/restricted-data-for-reviewer-role.png
     :class: no-shadow

* **Allow editors to select/update/delete any article**

  .. thumbnail:: ../../../img/graphql/manual/auth/editor-delete-permissions.png
     :class: no-shadow
  
  We can define straightforward permission rules for the ``editor`` role that allows access to any article without any checks. The rule shown in the above image will also work for select and update.



























