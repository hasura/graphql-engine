# Setup tables

1. Create the table using the console:

```
Table name: notes

Columns:
id: Integer auto-increment
note: Text

Table name: note_revision

Columns:
id: Integer auto-increment
note: Text
note_id: Integer (foreign key to notes.id)
update_at: Timestamp, default `now()`

```

# Setup AWS Lambda
Create a lambda function in AWS. This will be our webhook.

1. In this folder, run `npm install`
2. Then create a zip: `zip -r hge-mutation.zip .`
3. Create a Lambda function.
4. Select Node.js 6 as the runtime.
5. Select "start from scratch".
6. Add API gateway as a trigger.
7. Add an API to API gateway.
8. Upload the zip from previous step. The handler function of your lambda will be `index.handler`.
9. Add the following enviroment variables in your lambda config:
   1. `ADMIN_SECRET`: this is the admin secret key you configured when you setup HGE.
   2. `HGE_ENDPOINT`: the URL on which you HGE instance is running.

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
