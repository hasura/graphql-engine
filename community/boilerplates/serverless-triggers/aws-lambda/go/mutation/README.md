# Golang Mutation Example for AWS Lambda

This example trigger updates (executes a mutation) when an event occurs on
another table, written in Golang.

### Create tables

```
Table name: note

Columns:

id     Integer auto-increment
note   Text

Primary key: id
```

```
Table name: note_revision

Columns:

id         Integer auto-increment
note       Text
note_id    Integer
update_at  Timestamp, default: now()

Primary key: id
```

### Setup AWS Lambda
Create a Lambda function in AWS. This will be our webhook.

1. Create a function.
2. Select "start from scratch".
3. Select go 1.x as the runtime.
4. Add API gateway as a trigger.
5. Define the API endpoint on the API gateway.
6. Add the code in `mutation.go`.
7. Add environment variable `HGE_ENDPOINT` which is the Hasura GraphQL Engine endpoint.

### Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.

### Test the trigger

Goto `Data` tab on Hasura console, browse to `note` table and to the Browse rows
tab. Edit an existing note and check if the `note_revision` entry has been
created. Also, checkout the trigger request and response.

Trigger payload (request):
```json
{
    "event": {
        "op": "UPDATE",
        "data": {
            "old": {
                "note": "note1",
                "id": 1
            },
            "new": {
                "note": "note1 updated",
                "id": 1
            }
        }
    },
    "created_at": "2018-10-02T06:38:22.67311Z",
    "id": "f57a1c79-72ba-4c19-8791-37d1b9616bcf",
    "trigger": {
        "name": "note_revision_trigger",
        "id": "5d85cbd1-c134-45ce-810c-7ecd3b4fc1ee"
    },
    "table": {
        "schema": "public",
        "name": "note"
    }
}
```

Webhook response:
```json
{
    "data": {
        "insert_note_revision": {
            "affected_rows": 1,
            "returning": [
                {
                    "id": 2
                }
            ]
        }
    }
}
```
