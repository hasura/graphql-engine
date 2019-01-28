# Setup tables
1. Create the following tables using the console:

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

1. Create a function.
2. Select Python 3.6 as the runtime.
3. Select "start from scratch".
4. Add API gateway as a trigger.
5. Add an API to API gateway.
6. Add the code in `mutation.py`. The handler function of your lambda will be the `mutation.lambda_handler`.
7. Add the following enviroment variables in your lambda config:
   1. `ACCESS_KEY`: this is the access key you configured when you setup HGE.
   2. `HGE_ENDPOINT`: the URL on which you HGE instance is running.

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
