# Event trigger boilerplates on Zeit Now

Contributions are welcome for boilerplates in other languages.

### Setup Hasura GraphQL Engine

Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

[![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

Checkout [docs](https://hasura.io/docs/1.0/graphql/manual/deployment/index.html) for other deployment options.


###  Setup `now`

1. Create a Zeit account @ https://zeit.co/
2. Download [now cli](https://zeit.co/download#now-cli):
   ```bash
   npm install -g now
   ```
3. Login to `now`:
   ```bash
   now login
   ```

### Setup and deploy the trigger

Checkout any of the example triggers given below for further steps:

- [NodeJS Echo](nodejs/echo)
- [NodeJS Mutation](nodejs/mutation)
- [Go Echo](go/echo)
- [Go Mutation](go/mutation)
<!--
- [Python 3 Echo](python/echo)
- [Python 3 Mutation](python/mutation)
-->
