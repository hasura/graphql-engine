---
title: "Create Event Trigger"
---

Event triggers can be created using the Hasura console.

Open the Hasura console, head to the Events tab and click on the Create trigger button to open up the interface below to create an event trigger:

## Add

Give a name for the event trigger (say send_email) and select the table `users` and select the operation `insert`.

Click on `Create`.

![Create event trigger](/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/add-event-trigger.png)

## Try it out

To test this, we need to insert a new row into users table.

Head to Console -> Data -> users -> Insert Row and insert a new row.

Now head to Events tab and click on `send_email` event to browse the processed events.

![Test event trigger](/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/test-event-trigger.png)

Now everytime a new row is inserted into `users` table this event would be invoked.

