# Google Cloud Functions + NodeJS + Apollo

This is a GraphQL backend boilerplate in nodejs that can be deployed on Google Cloud Functions.

## Stack

node 8.10

Google Cloud Functions

#### Frameworks/Libraries

Apollo Server (GraphQL framework)

## Schema


```
type Query {
  hello:  String
}
```

## Local Development

The sample source code is present in `index.js`.

```bash
$ git clone git@github.com:hasura/graphql-engine
$ cd graphql-engine/community/boilerplates/remote-schemas/google-cloud-functions/nodejs
```

Start a local development server (you may need to install dependencies from npm):

```bash
$ npm i --no-save apollo-server
$ node localDev.js

Output:

Server ready at http://localhost:4000/
```

This will start a local server on `localhost:4000`. You can hit the graphql service at `localhost:4000`. This opens a graphql playground where you can query your schema.

## Deployment

1. Install `gcloud` cli.

2. From the current directory, run the following command to deploy the function:

```bash
$ gcloud functions deploy hello-graphql --entry-point handler --runtime nodejs8 --trigger-http
```

3. Get the trigger URL from the above output:

```yaml
httpsTrigger:
  url: https://us-central1-hasura-test.cloudfunctions.net/hello-graphql
```

