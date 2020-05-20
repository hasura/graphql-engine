.. meta::
   :description: Troubleshoot Hasura GraphQL engine errors
   :keywords: hasura, docs, error, troubleshooting

.. _troubleshooting:

Troubleshooting Hasura GraphQL engine errors
============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This section provides references that can help in troubleshooting errors when developing with Hasura.

Logs
----

In order to find out about the origins of an error, it can be helpful to check the logs.

Server logs
^^^^^^^^^^^

For errors that come from the Hasura server, server logs will give you more insights.
For details on how you can access these logs, as well as different log types, visit :ref:`this page <hge_logs>`.

Console logs
^^^^^^^^^^^^

Should there be any error coming from the Hasura console UI, they will show up in the Browser dev tools.

Remote service logs
^^^^^^^^^^^^^^^^^^^

Errors can come from a remote service, such as :ref:`remote schemas <remote_schemas>`, :ref:`events <event_triggers>` or :ref:`actions <actions>`. 
In this case, check the errors of the respective remote service. For actions, check out this :ref:`debugging page <debugging_actions>`.

GitHub issues
-------------

It's possible that someone with the same problem has created a GitHub issue on the `Hasura repo <https://github.com/hasura/graphql-engine/issues>`__.
If you don't come across an issue with solution in your search, feel free to create a `new issue <https://github.com/hasura/graphql-engine/issues/new>`__.

Hasura blog
-----------

The `Hasura blog <https://hasura.io/blog/>`__ contains a lot of useful resources including tutorials. 
You can use the search functionality to find what you're looking for.

.. admonition:: YouTube

  If you prefer to watch tutorials in the form of videos, check out the `Hasura Youtube channel <https://www.youtube.com/channel/UCZo1ciR8pZvdD3Wxp9aSNhQ>`__.

Postgres docs
-------------

If you come across a Postgres error, it will be helpful to check the `Postgres documentation <https://www.postgresql.org/docs/current/index.html>`__.

Discord
-------

If you didn't find a solution in any of the abovementioned sections or if you prefer to troubleshoot with the community,
feel free to join our `Discord server <http://hasura.io/discord>`__. Other users might have come across the same issues, 
and the Hasura community on Discord is very active and helpful. 

