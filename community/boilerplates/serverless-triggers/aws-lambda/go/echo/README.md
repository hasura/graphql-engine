# Go Echo Example for AWS Lambda

### Setup tables
1. Create table:

```
notes:
  id: Integer (auto-increment)
  note: Text

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

  ii. Upload *echo.zip* in the lambda console.
10. Click the **save** button for the changes to take effect.
11. Create an API with a lambda proxy integration as specified in the [AWS Documentation](https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-api-as-simple-proxy-for-lambda.html#api-gateway-create-api-as-simple-proxy-for-lambda-build).

### Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
