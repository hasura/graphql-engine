# GraphQL Transactions over WebSocket Protocol

Running GraphQL queries and mutations in a transaction over a WebSocket connection.

## Connection

The subprotocol name is `graphql-tx`.

```http
GET /v1/graphql HTTP/1.1
Host: localhost:8080
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
Sec-WebSocket-Version: 13
Sec-WebSocket-Protocol: graphql-tx
```

## Client - Server communication

All messages between client and server are [JSON](https://www.json.org) objects with a
compulsory `type` key and an optional `payload` key. The `type` represents the kind of message and
the `payload` has relevant data. Few messages from server has `id` key which is a unique WebSocket
id and `request_id` for query responses.

#### 1. Init

The `init` message is sent to initialise the transaction. It is identical in behaviour of PostgreSQL `BEGIN` command.
A Postgres transaction is initiated. You can set the session variables using `headers` key and the [transaction isolation level](https://www.postgresql.org/docs/current/transaction-iso.html)
using `isolation` key in the `payload`. Only one transaction is allowed in a single WebSocket connection.

- `type: init`
- `payload:`
    * `isolation?: any of 'read-committed'(default), 'serializable' and 'repeatable-read'`
    * `headers?: JSON object with String values`


Client Message:-
```json
{
  "type": "init",
  "payload": {
    "isolation": "serializable",
    "headers": {
      "X-Hasura-Role": "user",
      "X-Hasura-User-Id": "1"
    }
  }
}
```
Server Message on Success:-
```json
{
  "type": "initialised"
}
```
Server Message on Error:-
```json
{
  "type": "init_error",
  "payload": "transaction cannot be initialised more than once in a single WebSocket session"
}
```

#### 2. Execute

The `execute` message is sent to execute a GraphQL query or mutation. Mutations executed will not be committed in the database
until `commit` message is sent. The `request_id` in `payload` is used to identify the responses from the server for the queries
sent by the client.

- `type: execute`
- `payload: Object`
    * `request_id?: String`
    * `query: GraphQL query object`

Client Message:-
```json
{
  "type": "execute",
  "payload": {
    "request_id": "some-unique-id",
    "query": {
      "query": "{query q($id: Int){test(where: {id: {_eq: $id}}){id name}}}",
      "operationName": "q",
      "variables": {
        "id": 1
      }
    }
  }
}
```
Server Message on Success:-
```json
{
  "type": "data",
  "id": "c49fcf59-498f-4574-9c22-d03a5ca364d9",
  "request_id": "some-unique-id",
  "payload": {
    "data": {
      "test": [
        {
          "id": 1,
          "name": "Clarke"
        }
      ]
    }
  }
}
```
Server Message on Error:-
```json
{
  "type": "error",
  "id": "c49fcf59-498f-4574-9c22-d03a5ca364d9",
  "request_id": "some-unique-id",
  "payload": {
    "errors": [
        {
          "message": "Remote server queries are not supported over graphql transactions",
          "extensions": {
              "code": "not-supported",
              "path": "$"
          }
        }
      ]
  }
}
```

#### 3. Commit

The `commit` message is send to commit the current transaction. All mutations made in the transaction are committed in the database.
The message is identical in behaviour of PostgreSQL `COMMIT` command. The WebSocket connection is terminated on this message.

Client Message:-
```json
{
  "type": "commit"
}
```

Server Message:-
```json
{
  "type": "close",
  "id": "c49fcf59-498f-4574-9c22-d03a5ca364d9",
  "payload": "Executed 'COMMIT' command"
}
```


#### 4. Abort

The `abort` message is sent to roll back the current transaction and causes all the mutations made in the transaction
to be discarded. This message is identical in behaviour of PostgreSQL `ABORT` command. The WebSocket connection is
terminated on this message.

Client Message:-
```json
{
  "type": "abort"
}
```
Server Message:-
```json
{
  "type": "close",
  "id": "c49fcf59-498f-4574-9c22-d03a5ca364d9",
  "payload": "Executed 'ABORT' command"
}
```
