Using serverless functions
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can use serverless functions along with Event Triggers to design an async business workflow without
having to manage any dedicated infrastructure.

As Hasura event triggers can deliver database events to any webhook, serverless functions can be perfect candidates
for their handlers.

Why use serverless functions?
-----------------------------
1. Cost effectiveness.
2. No infra management.
3. Async business logic.

Examples
--------

You can find a bunch of examples for various serverless cloud providers in this repo:

https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers.

For example: update related data on a database event
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this example we make a note taking app. Whenever a user updates their note, we want to store a revision of that
note in a separate table.

You can find the complete example at:

https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/aws-lambda/nodejs6/mutation.

Consider the following simplified schema for the above:

.. code-block:: SQL
   
    notes (
      id INT PRIMARY KEY,
      note TEXT
    )

    note_revision (
      id INT PRIMARY KEY,
      note TEXT,
      note_id INT FOREIGN KEY REFERENCES notes(id),
      update_at TIMESTAMP DEFAULT now()
    )


Whenever an update happens to the ``notes`` table, we want to insert a row into the ``note_revision`` table.

For this we :doc:`setup an event trigger <create-trigger>` on ``UPDATE`` to the ``notes`` table which calls an
AWS Lambda function. The AWS Lambda function itself uses a GraphQL mutation to insert a new row into the
``note_revision`` table. As the :doc:`event trigger payload <payload>` in case of updates gives us both the old and
the new data, we can store the old note data in our revision table.

Our AWS Lambda code looks like this:

.. code-block:: javascript

    // Lambda which gets triggered on insert, and in turns performs a mutation

    const fetch = require('node-fetch');

    const adminSecret = process.env.ADMIN_SECRET;
    const hgeEndpoint = process.env.HGE_ENDPOINT;

    const query = `
      mutation updateNoteRevision ($noteId: Int!, $data: String!) {
        insert_note_revision (objects: [
          {
            note_id: $noteId,
            note: $data
          }
        ]) {
          affected_rows
        }
      }
    `;

    exports.handler = (event, context, callback) => {
        let request;
        try {
            request = JSON.parse(event.body);
        } catch (e) {
            return callback(null, {statusCode: 400, body: "cannot parse hasura event"});
        }

        const response = {
            statusCode: 200,
            body: "success"
        };
        const qv = {noteId: request.data.old.id, data: request.data.old.note};
        fetch(hgeEndpoint + '/v1/graphql', {
            method: 'POST',
            body: JSON.stringify({query: query, variables: qv}),
            headers: {'Content-Type': 'application/json', 'x-hasura-admin-secret': adminSecret},
        })
            .then(res => res.json())
            .then(json => {
                console.log(json);
                callback(null, response);
            });
    };
