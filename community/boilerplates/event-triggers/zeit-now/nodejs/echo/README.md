# NodeJS Echo Example on Zeit

If you haven't already, follow the first part of instructions [here](../../).

This is an example which echoes the event back, written in NodeJS.

### Create a table

Visit the GraphQL Engine URL to open console.

Goto `Data` tab.

Create the following table:

```
Table name: note

Columns:

id     Integer auto-increment
note   Text

Primary key: id
```

### Deploy the function

Execute `now` in this directory:

```bash
now
```

Once the deployment is done, you'll get an endpoint like
`https://zeit-echo-hhasdewasd.now.sh`. Note this down as `NOW_URL`. 

> **Note**: Now.sh deployments are immutable, i.e. each time you deploy the
> code, a new URL will be provisioned. You can [alias the
> deployment](https://zeit.co/docs/getting-started/assign-a-domain-name#1.-using-a-now.sh-domain)
> to keep a constant URL. 

### Create the trigger

Goto Hasura console, `Events` tab and create a new trigger:

```
Trigger name: note_trigger

Schema/Table: public/note

Operations: Insert, Update, Delete

Webhook URL: NOW_URL
```

Replace `NOW_URL` with the URL noted earlier.


### Test the trigger

Goto `Data` tab on Hasura console, browse to `note` table and insert a new row.
Once a new row is inserted, goto `Events` table and `note_trigger`. Checkout the
request and response body for the processed events.

Trigger payload (request):
```json
{
    "event": {
        "op": "INSERT",
        "data": {
            "old": null,
            "new": {
                "text": "new-entry",
                "id": 1
            }
        }
    },
    "created_at": "2018-10-01T17:21:03.76895Z",
    "id": "b30cc7e6-9f3b-48ee-9a10-16cce333df40",
    "trigger": {
        "name": "note_trigger",
        "id": "551bd6a9-6f8b-4644-ba7f-80c08eb9227b"
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
    "message": "received 'b30cc7e6-9f3b-48ee-9a10-16cce333df40' for 'INSERT' operation on 'note' table in 'public' schema from 'note_trigger' trigger",
    "oldData": null,
    "newData": {
        "text": "new-entry",
        "id": 1
    }
}
```

