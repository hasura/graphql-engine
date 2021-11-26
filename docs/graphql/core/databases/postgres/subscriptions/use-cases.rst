.. meta::
   :description: Use cases for Hasura subscriptions
   :keywords: hasura, docs, subscription, use case

.. _subscription_use_cases:

Subscriptions sample use cases
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The following are a few use cases for using subscriptions:

.. _subscribe_field:

Subscribe to the latest value of a particular field
---------------------------------------------------

In case you are interested only in the latest value of a particular field, you can use subscriptions to fetch the
field and get updated with its latest value whenever it changes.

Example: Live location tracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use subscriptions to show the current location of a vehicle on a map.

Let's say we have the following database schema:

.. code-block:: sql

  vehicle (
    id INT PRIMARY KEY,
    vehicle_number TEXT
  )

  vehicle_location (
    id INT PRIMARY KEY,
    location TEXT,
    time_stamp TIMESTAMP,
    /* used to create relationship 'locations' for vehicle */
    vehicle_id INT FOREIGN KEY REFERENCES vehicle(id)
  )

Now we can use the following subscription to fetch the latest location of a vehicle to display it on a map:

.. graphiql::
  :view_only:
  :query:
    # $vehicleId = 3
    subscription getLocation($vehicleId: Int!) {
      vehicle_location(where: {id: {_eq: $vehicleId}}) {
        id
        locations {
          vehicle_number       
        }
	location
      }
    }
  :response:
    {
      "data": {
        "vehicle_location": [
          {
            "id": 3,
            "locations": [
              {
                "vehicle_number": "KA04AD4583"
              },
	      "location": "(31.1657, 10.4115)"
            ]
          }
        ]
      }
    }
  :variables:
    {
     "$vehicleId": 3
    }

Check this `sample app <https://realtime-location-tracking.demo.hasura.app/>`__ for a working demo
(`source code <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/realtime-location-tracking>`__).

.. _subscribe_table:

Subscribe to changes to a table's entries
-----------------------------------------

In case you are interested in all the additions/changes to a table's entries, you can use subscriptions to fetch the
table rows and get updates whenever there are any additions/changes to the table.

Example: Chat app
^^^^^^^^^^^^^^^^^

Use subscriptions to show new messages in a chatroom.

Let's say we have the following database schema:

.. code-block:: sql

  user (
    id INT PRIMARY KEY,
    username TEXT UNIQUE
  )

  message (
    id INT PRIMARY KEY,
    texts TEXT,
    time_stamp TIMESTAMP default now(),
    /* used to create relationship 'author' with the 'user' table */
    user_id INT FOREIGN KEY REFERENCES user(id)
  )

Now we can use the following subscription to display the latest messages in a chatroom:

.. graphiql::
  :view_only:
  :query:
    subscription getMessages {
      message(order_by: {timestamp: desc}) {
        texts
        time_stamp
        author {
          username
        }
      }
    }
  :response:
    {
      "data": {
        "message": [
          {
            "texts": "I am fine, and you?",
            "timestamp": "2021-11-29T07:42:56.689135",
            "user": {
              "username": "Jane"
            }
          },
          {
            "text": "Hi! How are you?",
            "timestamp": "2021-11-29T07:42:19.506049",
            "user": {
              "username": "Musk"
            },
          },
          {
            "text": "Hi!",
            "timestamp": "2021-11-29T07:38:52.347136",
            "user": {
              "username": "Jane"
            }
          }
        ]
      }
    }

Check this `sample app <https://realtime-chat.demo.hasura.app/>`__ for a working demo
(`source code <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/realtime-chat>`__).

.. _subscribe_derived:

Subscribe to the latest value of some derived data
--------------------------------------------------

In case you are interested in the latest value of some derived data, you can :ref:`create a view to query the derived
data <custom_views>` and then use subscriptions to fetch the derived value and get its latest value
whenever it updates.

Example: A poll dashboard
^^^^^^^^^^^^^^^^^^^^^^^^^

Use subscriptions to show the result of a poll.

Let's say we have the following database schema:

.. code-block:: sql

  poll (
    id INT PRIMARY KEY,
    question TEXT
  )

  option (
    id INT PRIMARY KEY
    poll_id INT FOREIGN KEY REFERENCES poll(id)
    text TEXT
  )

  user (
    id INT PRIMARY KEY
    name TEXT
  )

  vote (
    id INT PRIMARY KEY,
    option_id INT FOREIGN KEY REFERENCES option(id),
    user_id INT FOREIGN KEY REFERENCES user(id),
    timestamp TIMESTAMP
  )

First, create a view ``poll_results`` to give the result of the poll:

.. code-block:: sql

  CREATE OR REPLACE VIEW public."poll_results" AS
    SELECT poll.id AS poll_id,
           o.option_id,
           count(*) AS votes
      FROM (
        (
          SELECT vote.option_id,
                 option.poll_id,
                 option.text
            FROM (
              vote
                LEFT JOIN option ON ((option.id = vote.option_id))
            )
          ) o
              LEFT JOIN poll ON ((poll.id = o.poll_id))
        )
    GROUP BY poll.question, o.option_id, poll.id;

This view will have the following fields: ``poll_id``, ``option_id`` and ``votes``, i.e. it gives the number of votes
received by each option for a poll.

Next, :ref:`set up relationships <table_relationships>` ``poll`` and ``option`` between the ``poll_results`` view
and the ``poll`` and ``option`` tables using the ``poll_id`` and ``option_id`` fields respectively.

Now we can use the following subscription to display the latest poll result:

.. graphiql::
  :view_only:
  :query:
    # $pollId = 1
    subscription getResult($pollId: Int!) {
      poll_results (
        where: { poll_id: {_eq: $pollId} }
      ) {
        poll_id
        option {
          text
        }
        votes
      }
    }
  :response:
    {
      "data": {
        "poll_results": [
          {
            "poll_id": 1,
            "votes": 1,
            "option": {
              "text": "Pizza"
            }
          },
          {
            "poll_id": 1,
            "votes": 1,
            "option": {
              "text": "Salad"
            }
          },
          {
            "poll_id": 1,
            "votes": 2,
            "option": {
              "text": "Sandwich"
            }
          },
          {
            "poll_id": 1,
            "votes": 3,
            "option": {
              "text": "Burger"
            }
          },
          {
            "poll_id": 1,
            "votes": 1,
            "option": {
              "text": "Lasagna"
            }
          }
        ]
      }
    }

Check this `sample app <https://realtime-poll.demo.hasura.app/>`__ for a working demo
(`source code <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/realtime-poll>`__).
