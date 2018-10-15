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

1. Sign in to the AWS Management Console and open the AWS Lambda console.
2. Choose "Create a function" under the **Get Started** section.
3. Select "Author from scratch".
4. Specify the **Name** for your lambda.
5. Select go 1.x as the **Runtime**.
6. In **Role** choose **Create new role from template(s)**  
7. In **Role name**, enter a name for your role, leave the **Policy Templates** field blank.
8. Build and upload the code to AWS Lambda:

  i. Run the bash script provided

  `bash build.sh`

  ii. Upload *mutation.zip* in the lambda console.
9. In the Designer pane, add an environment variable `HGE_ENDPOINT` which is the Hasura GraphQL Engine endpoint.
10. Click the **save** button for the changes to take effect.
11. Create an API with a lambda proxy integration as specified in the [AWS Documentation](https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-api-as-simple-proxy-for-lambda.html#api-gateway-create-api-as-simple-proxy-for-lambda-build).

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
