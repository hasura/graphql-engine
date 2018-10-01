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
2. Select "start from scratch".
3. Select go 1.x as the runtime.
4. Add API gateway as a trigger.
5. Define the API endpoint on the API gateway.
6. Add the code in `echo.go`.

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
