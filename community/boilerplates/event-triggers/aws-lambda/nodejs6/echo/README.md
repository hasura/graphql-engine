# Setup tables
1. Create table:

```
notes:
  id: int
  note: text
```

# Setup AWS Lambda
Create a lambda function in AWS. This will be our webhook.

1. Create a function.
2. Select Node.js 6 as the runtime.
3. Select "start from scratch".
4. Add API gateway as a trigger.
5. Add an API to API gateway.
6. Add the code in `index.js`. The handler function of your lambda will be the `index.handler`.

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
