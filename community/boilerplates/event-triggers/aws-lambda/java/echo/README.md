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
2. Select Java 8 as the runtime.
3. Select "start from scratch".
4. Add API gateway as a trigger.
5. Add an API to API gateway.
6. Edit the code in the `handleRequest` method in `/src/main/java/example/Hello.java`.

# Deploy AWS Lambda

1. In terminal go into project: `cd community/boilerplates/serverless-triggers/aws-lambda/java/echo`
2. Build deployment package: `mvn package`
2. Upload `target/java-lambda-1.0-SNAPSHOT.jar` using AWS console.


# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
