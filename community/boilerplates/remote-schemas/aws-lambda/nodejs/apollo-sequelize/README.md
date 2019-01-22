# AWS Lambda + NodeJS + Apollo + Sequelize

This is a GraphQL backend boilerplate in nodejs that can be deployed on AWS Lambda.

## Stack

node 8.10

Postgres

AWS Lambda

#### Frameworks/Libraries

Apollo Server (GraphQL framework)

Sequelize (Postgres ORM)

## Schema

We consider a user schema where a user can be added only if a custom validation passes. The custom validation involves fetching a min amount from a table and checking if the user balance is greater than the min amount. This will be done in a transaction.

```
type Query {
  hello:  String
}

type Mutation {
  validateAndAddUser(name: String, balance: Int): User
}

type User {
  id:       Int
  name:     String
  balance:  Int
}
```

## Local Development

The sample source code is present in `index.js`. Clone the repo and go to `community/boilerplates/remote-schemas/remote-schema/aws-lambda/nodejs/apollo-sequelize` folder:

```bash
$ git clone git@github.com:hasura/graphql-engine
$ cd graphql-engine/community/boilerplates/remote-schemas/remote-schema/aws-lambda/nodejs/apollo-sequelize
```

1) First, let's set the environment variable for connecting to the postgres instance. This can be a local postgres instance or some managed postgres instance like AWS RDS.

```bash
$ export POSTGRES_CONNECTION_STRING='postgres://username:password@rds-database-endpoint.us-east-1.rds.amazonaws.com:5432/mydb' 
```

2) Next, lets create the tables required for our schema.

```bash
psql $POSTGRES_CONNECTION_STRING -c "create table users(id serial primary key, name text, balance integer); create table min_amount(amount integer); insert into min_amount values (100)" 
```

3) Now, you can start a development environment by setting an environment variable and running the server:

```bash
$ LAMBDA_LOCAL_DEVELOPMENT=1 node index.js

Output:

Server ready at http://localhost:4000/
```

This will start a local server on `localhost:4000`. You can hit the graphql service at `localhost:4000`. This opens a graphql playground where you can query your schema.

Now, you can play with the schema and make any changes in the source code for additional functionalities as you desire.

## Deployment

Now that you have run the graphql service locally and made any required changes, it's time to deploy your service to AWS Lambda and get an endpoint. The easiest way to do this is through the AWS console.

1) Create a Lambda function by clicking on Create Function on your Lambda console. Choose the `NodeJS 8.10` runtime and `lambda_basic_execution` role.

![create-lambda](../../../_assets/create-lambda.png)

2) In the next page (or Lambda instance page), select API Gateway as the trigger.

![create-api-gateway](../../../_assets/create-api-gateway.png)

3) Configure the API Gateway as you wish. The simplest configuration is shown below.

![configure-api-gateway](../../../_assets/configure-api-gateway.png)

Save your changes. You will receive a HTTPS endpoint for your lambda.

![output-api-gateway](../../../_assets/output-api-gateway.png)

If you go to the endpoint, you will receive a "Hello from Lambda!" message. This is because we haven't uploaded any code yet!

4) Zip and upload code. Make sure to set the handler as `index.handler` and add the `POSTGRES_CONNECTION_STRING` environment variable:

```bash
$ zip -r graphql.zip *
```

And that's it. Hit save and visit the endpoint again. You will see the graphql playground again.

**IMPORTANT NOTE:** You may have to edit the GraphQL URL in the Playground to reflect the right endpoint ( same as the URL created by the API Gateway ). 
