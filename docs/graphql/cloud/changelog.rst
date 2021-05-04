.. meta::
   :description: Hasura Cloud Changelog
   :keywords: hasura, docs, cloud, changelog

.. _cloud_changelog:

Hasura Cloud Changelog
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Please find the changelog (includes features, bug fixes, breaking change notifications, etc) of Hasura Cloud versions here. Note that Hasura Cloud includes everything from Hasura Open Source, hence the corresponding open source changelog is included in every version.  

v2.0.0-cloud.1
--------------

Includes everything from OSS ``v2.0.0-alpha.1`` through ``v2.0.0-alpha.6``: https://github.com/hasura/graphql-engine/releases

- Support centralized PG as the metadata database in cloud
- Allow adding data source using env var
- Add cache rate limiting to the query cache.
- Add per query family cache metrics.
- Add session variables to query cache keys.
- Add endpoints for clearing the query cache and reading the query cache metrics.
- Add read replicas UI

Breaking changes
^^^^^^^^^^^^^^^^

This version includes breaking changes from previous version (i.e ``v1.3.3-cloud.x``).

- Multiple mutations in same request are not transactional: This will be fixed in one of the upcoming versions.

- Fixes behaviour of ``null`` in ``where`` expressions: The semantics of explicit ``null`` values in ``where`` filters have changed according to the discussion in issue 704: an explicit ``null`` value in a comparison input object will be treated as an error rather than resulting in the expression being evaluated to ``True``. For instance: ``delete_users(where: {id: {_eq: $userId}}) { name }`` will yield an error if ``$userId`` is ``null`` instead of deleting all users.

- Change in ``null`` join semantics in remote schema relationships : The remote schema will be queried only when all of the joining arguments are not ``null`` values. When there are ``null`` value(s) in any of the join arguments, the remote schema won't be queried and the response of the remote relationship field will be ``null``. Earlier, the remote schema was queried with the ``null`` value arguments and the response depended upon how the remote schema handled the null arguments but as per user feedback, this behaviour was clearly not expected.

- Older Hasura versions as Remote Schemas: With v2.0, some of the auto-generated types have been extended. For example, ``String_comparison_exp`` has an additional ``regex`` input object field, this means if you have an older version of Hasura added as a Remote Schema then it will have a type conflict. You should upgrade all Hasura Remote Schemas to avoid such type conflicts.
