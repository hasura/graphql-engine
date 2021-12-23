# Setup tables

1. Create table:

```
profile (
  id INT PRIMARY KEY,
  name TEXT
)
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


# Add the trigger in Hasura GraphQL

1. In events tab, add a trigger
1. Select all insert, update, delete operations for the trigger.
1. Paste the API endpoint of your AWS lambda as the webhook.

# Test your integration

1. Create a record.
1. Update a record.
1. Delete a record.
1. Check the logs (in the Events tab) to see what is going on.
