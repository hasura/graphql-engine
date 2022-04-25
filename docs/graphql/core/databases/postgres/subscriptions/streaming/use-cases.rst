.. meta::
   :description: Use cases for Hasura streaming subscriptions
   :keywords: hasura, docs, subscription, use case, streaming

.. _pg_streaming_subscriptions_use_cases:

Streaming subscriptions sample use cases
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The following are a few use cases for using streaming subscriptions:

.. _pg_streaming_changed_events:

Subscribing only to the events that have been changed
-----------------------------------------------------

In case you are interested only in the latest events, you can use streaming subscriptions
to fetch those events.

.. _pg_get_undelivered_messages:

Get the undelivered messages in a chat application
--------------------------------------------------

Consider the following schema:

.. code-block:: sql

   messages (
     id serial primary key,
     from_id uuid references users(id),
     to_id uuid references users,
     content text,
     status text,
     created_at timestamptz default now(),
   )

   users (
     id uuid primary key,
     first_name text,
     last_name text,
     created_at timestamptz default current_timestamp
   )

and the following messages need to be streamed:

.. code-block:: json

     [
       {
         "id": 432432,
         "from": {
           "first_name": "Cindy"
         },
         "to": {
           "first_name": "Michael"
         },
         "content": "Heyyy",
         "created_at": "2020-01-01 01:00:00"
       },
       {
         "id": 432433,
         "from": {
           "first_name": "Michael"
         },
         "to": {
           "first_name": "Cindy"
         },
         "content": "Heyy! How are you?",
         "created_at": "2020-01-01 01:01:20"
       },
       {
         "id": 432432,
         "from": {
           "first_name": "Cindy"
         },
         "to": {
           "first_name": "Michael"
         },
         "content": "I'm good! What about you?",
         "created_at": "2020-01-01 01:00:00"
       },
       {
         "id": 432433,
         "from": {
           "first_name": "Michael"
         },
         "to": {
           "first_name": "Cindy"
         },
         "content": "All good here too! Thanks",
         "created_at": "2020-01-01 01:01:20"
       }
     ]

To stream the latest undelivered messages:

.. code-block:: graphql

    subscription getUndeliveredMessages {
       # will get all the messages that have `created_at > 2022-01-01` in batches of 100 rows
       messages_stream(cursor: {created_at: "2022-01-01", ordering: ASC}, batch_size: 2) {
         id
         from {
           first_name
         }
         to {
           first_name
         }
         content
         created_at
       }
     }

The first response sent will be:

.. code-block:: json

      {
        "data": {
           "messages_stream": [
              {
                "id": 432432,
                "from": {
                  "first_name": "Cindy"
                },
                "to": {
                  "first_name": "Michael"
                },
                "content": "Heyyy",
                "created_at": "2020-01-01 01:00:00"
              },
              {
                "id": 432433,
                "from": {
                  "first_name": "Michael"
                },
                "to": {
                  "first_name": "Cindy"
                },
                "content": "Heyy! How are you?",
                "created_at": "2020-01-01 01:01:20"
              }
           ]
        }
      }

The next response sent will be the following, note that the messages sent
have ``created_at > 2020-01-01 01:01:20``, the greatest value of the cursor
column sent in the previous response.

.. code-block:: json

      {
        "data": {
           "messages_stream": [
              {
                "id": 432432,
                "from": {
                  "first_name": "Cindy"
                },
                "to": {
                  "first_name": "Michael"
                },
                "content": "I'm good! What about you?",
                "created_at": "2020-01-01 01:00:00"
              },
              {
                "id": 432433,
                "from": {
                  "first_name": "Michael"
                },
                "to": {
                  "first_name": "Cindy"
                },
                "content": "All good here too! Thanks",
                "created_at": "2020-01-01 01:01:20"
              }
           ]
        }
      }

.. toctree::
   :maxdepth: 1
   :hidden:
