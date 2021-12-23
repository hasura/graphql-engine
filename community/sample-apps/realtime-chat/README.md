# Realtime Chat using GraphQL Subscriptions

This is the source code for a fully working group chat app that uses subscriptions in Hasura GraphQL Engine. It is built using React and Apollo.

Run this example with Docker: `docker compose up -d --build`

[![Edit chat-app](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/realtime-chat?fontsize=14)

- [Fully working app](https://realtime-chat.demo.hasura.app/)
- [Backend](https://realtime-chat.demo.hasura.app/console)

Adapted from the [original blogpost by Rishichandra Wawhal](https://hasura.io/blog/building-a-realtime-chat-app-with-graphql-subscriptions-d68cd33e73f).

[![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

## TLDR

- Hasura allows us to build a realtime GraphQL API without writing any backend code.

- To save bandwith we use a subscription to listen for message events, then run a query to fetch only the new messages.

- Every two seconds our frontend runs a user_online mutation to populate an online users list.

- A user_typing mutation is run whenever we type few characters and a subscription is used to fetch the last typing user.

## Introduction

Hasura allows us to instantly create a realtime GraphQL API from our data. In this tutorial we walk through creating a group chat application, without needing to write any backend code, using React and Apollo. The focus is on data models that we store in Postgres rather than full chat functionality.

## Data Modelling

### Users

When a user signs up we insert their chosen username and generate their ID. We also track when they last typed and were seen.

```sql
user (
  id SERIAL PRIMARY KEY
  username TEXT UNIQUE
  last_seen timestamp with time zone
  last_typed timestamp with time zone
)
```

### Messages

For our tutorial we will just be inserting messages, not editing or deleting, but if we wanted to in the future Hasura will autogenerate the mutations for us. We could also extend this by adding features such as multiple different chatrooms.

```sql
message (
  id SERIAL NOT NULL PRIMARY KEY
  "text" TEXT NOT NULL
  username INT FOREIGN KEY REFERENCES user(username) NOT NULL
  "timestamp" timestamp with time zone DEFAULT now() NOT NULL
)
```

### Online users

To query the users online we create a Postgres view that fetches all users with last_seen less than 10 seconds ago.

```sql
CREATE OR REPLACE VIEW public."user_online" AS
SELECT "user".id,
  "user".username,
  "user".last_typed,
  "user".last_seen
FROM "user"
WHERE ("user".last_seen > (now() - '00:00:10'::interval));
```

### Typing Indicator

To query the last person typing we create a similar view with last_typed within the past 2 seconds.

```sql
CREATE OR REPLACE VIEW public."user_typing" AS
SELECT "user".id,
  "user".username,
  "user".last_typed,
  "user".last_seen
FROM "user"
WHERE ("user".last_typed > (now() - '00:00:02'::interval));
```

## Frontend

### Creating a user

At user signup we insert a single row into the users table

```gql
mutation ($username: String!) {
  insert_user_one(object: { username: $username }) {
    id
    username
  }
}
```

We take the returned id and username and store it in our app's state management.

### User online events

Every two seconds we run a mutation to update our user's last_seen value. For example, with React using UseEffect or componentDidMount we could call the mutation inside a setInterval. [Be mindful when using setInterval inside a React hook.](https://overreacted.io/making-setinterval-declarative-with-react-hooks/)

```gql
mutation ($userId: Int!) {
  update_user_by_pk(pk_columns: { id: $userId }, _set: { last_seen: "now()" }) {
    id
  }
}
```

### Subscribing to new messages

We can use Graphql subscriptions in two ways:

1. Subscribe to a query and render all the data that comes back.

1. Use the subscription as an event notification then run our own custom fetching logic.

Option two is best for our chatroom use case because we can do some work to just fetch new messages. Otherwise every new message we would also come with all previous ones.

1. On load we fetch all existing messages by setting $last_received_id to -1 and $last_received_ts to a timestamp well into the past such as '2018-08-21T19:58:46.987552+00:00'

   ```gql
   query ($last_received_id: Int, $last_received_ts: timestamptz) {
     message(
       order_by: { timestamp: asc }
       where: {
         _and: {
           id: { _neq: $last_received_id }
           timestamp: { _gte: $last_received_ts }
         }
       }
     ) {
       id
       text
       username
       timestamp
     }
   }
   ```

1. Now we subscribe to new message events

   ```gql
   subscription {
     message(order_by: { id: desc }, limit: 1) {
       id
       username
       text
       timestamp
     }
   }
   ```

1. Once we detect a new message we call our message query with our last known id and timestamp from our client state.

With a bit of magic from realtime subscriptions we saved our users a ton of data by only fetching new messages.

### Sending messages

A user sends a message by inserting a row into the messages table. In the future if we setup [JWT based authentication in the future](https://hasura.io/docs/latest/graphql/core/auth/authentication/jwt.html) we can automatically [set the username from the JWT custom claims](https://hasura.io/docs/latest/graphql/core/auth/authorization/roles-variables.html#dynamic-session-variables).

```gql
mutation insert_message($message: message_insert_input!) {
  insert_message_one(object: $message) {
    id
    timestamp
    text
    username
  }
}
```

### UI Typing Indicator

1. Similar to the user online events, when a user types a few characters we run a mutation on their last_typing timestamp.

   ```gql
   mutation ($userId: Int!) {
     update_user_by_pk(
       pk_columns: { id: $userId }
       _set: { last_typed: "now()" }
     ) {
       id
     }
   }
   ```

1. Then we subscribe to our Postgres view of the last user typing, making sure to exclude ourselves.

   ```gql
   subscription ($selfId: Int) {
     user_typing(
       where: { id: { _neq: $selfId } }
       limit: 1
       order_by: { last_typed: desc }
     ) {
       last_typed
       username
     }
   }
   ```

### Online Users list

We can easily get a list of all users online by subscribing to the user_online view we created.

```gql
subscription {
  user_online(order_by: { username: asc }) {
    id
    username
  }
}
```
