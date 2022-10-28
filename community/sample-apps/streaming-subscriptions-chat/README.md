# Realtime Chat using Streaming Subscriptions

This is the source code for a fully working group chat app that uses [streaming subscriptions](https://hasura.io/docs/latest/graphql/core/databases/postgres/subscriptions/streaming/index/) in Hasura GraphQL Engine. It is built using React and Apollo.

Run this example with Docker: `docker compose up -d --build`

- [Fully working app](https://eclectic-dragon-25a38c.netlify.app)

Adapted from the [original blogpost by Rishichandra Wawhal](https://hasura.io/blog/building-a-realtime-chat-app-with-graphql-subscriptions-d68cd33e73f).

[![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

## TLDR

- Hasura allows us to build a real-time GraphQL API without writing any backend code.

- Using [streaming subscriptions](https://hasura.io/docs/latest/graphql/core/databases/postgres/subscriptions/streaming/index/) we fetch the last ten messages then stream new messages.

- Every two seconds our frontend runs a user_online mutation to populate an online users list.

- A user_typing mutation is run whenever we type a few characters and a subscription is used to fetch the last typing user.

## Introduction

Hasura allows us to instantly create a real-time GraphQL API from our data. In this tutorial we walk through creating a group chat application without needing to write any backend code, using React and Apollo. The focus is on data models we store in Postgres rather than full chat functionality.

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

For our tutorial we will just be inserting messages, not editing or deleting, but if we wanted to in the future, Hasura would autogenerate the mutations. We could also extend this by adding features such as multiple different chatrooms.

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

We take the returned id and username and store them in our app's state management.

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

Using [streaming subscriptions](https://hasura.io/docs/latest/graphql/core/databases/postgres/subscriptions/streaming/index/) we fetch the last N messages (the example uses 10). Then new messages are streamed using graphql-ws.

```gql
# We can pass in how far back we want to fetch messages
subscription ($last_received_ts: timestamptz) {
  message_stream(
    cursor: { initial_value: { timestamp: $last_received_ts } }
    batch_size: 10
  ) {
    id
    username
    text
    timestamp
  }
}
```

With a bit of magic from streaming subscriptions we saved our users a ton of data by only fetching new messages.

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
