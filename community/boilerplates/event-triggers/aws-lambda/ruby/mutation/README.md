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

1. Create a function.
1. Select Ruby 2.5 as the runtime.
1. Select "Author from scratch".
1. Select "handler" as the function name.
1. Press "Create function".
1. Add API gateway as a trigger (in this example you can use Open as the security option).
1. Add an API to API gateway.
1. Add the code in `lambda_function.rb` to the lambda function editor. The handler function of your lambda will be the lambda_handler.
1. Add the following enviroment variables in your lambda config:
   1. `ACCESS_KEY`: this is the access key you configured when you setup HGE (`HASURA_GRAPHQL_ADMIN_SECRET` env variable).
   1. `HGE_ENDPOINT`: the URL on which you HGE instance is running.

# Add the trigger in Hasura GraphQL

1. In events tab, add a trigger
1. Select the "notes" table and the update trigger.
1. Paste the API endpoint of your AWS lambda as the webhook.

# Test your integration

1. Create a note.
1. Change the contents of the note.
1. Select all note revisions, previous note value should be visible.
1. Check the logs (in the Events tab) to see what is going on.
