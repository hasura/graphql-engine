# NodeJS Mutation Example on Zeit

If you haven't already, follow the first part of instructions [here](../../).

This example trigger updates (executes a mutation) when an event occurs on
another table, written in NodeJS.

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

### Deploy the function

Execute `now` in this directory with an env var required for the code to execute
a mutation:

```bash
now -e HGE_ENDPOINT=https://my-app.herokuapp.com/v1/graphql
```

`HGE_ENDPOINT` is the Hasura GraphQL Engine endpoint.

Once the deployment is done, you'll get an endpoint like
`https://zeit-mutation-hhasdewasd.now.sh`. Not this down as `NOW_URL`. 

> **Note**: Now.sh deployments are immutable, i.e. each time you deploy the
> code, a new URL will be provisioned. You can [alias the
> deployment](https://zeit.co/docs/getting-started/assign-a-domain-name#1.-using-a-now.sh-domain)
> to keep a constant URL. 

### Create the trigger

Goto Hasura console, `Events` tab and create a new trigger:

```
Trigger name: note_revision_trigger

Schema/Table: public/note

Operations: Update

Webhook URL: NOW_URL
```

Replace `NOW_URL` with the URL noted earlier.

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
    "result": {
        "data": {
            "insert_note_revision": {
                "returning": [
                    {
                        "__typename": "note_revision",
                        "id": 1
                    }
                ],
                "affected_rows": 1,
                "__typename": "note_revision_mutation_response"
            }
        }
    }
}
```
