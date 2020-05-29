# Sample Auth Webhook for Hasura GraphQL engine

This is a sample auth webhook for authenticating requests to the Hasura GraphQL engine.

It has boilerplate code written in `server.js` where you can handle authentication.

## Quick deploy

### Deploy with Heroku (recommended)

Run the following commands:

```
git clone https://github.com/hasura/graphql-engine
cp -r graphql-engine/community/boilerplates/auth-webhooks/nodejs-express <some-dir>
cd <some-dir>
git init && git add . && git commit -m "init auth webhook"
```

You need to setup a Heroku app:

```
heroku apps:create
git push heroku master
```

### Deploy using [Now](https://zeit.co/now)

Run the following commands to deploy using Now.

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/community/boilerplates/auth-webhooks/nodejs-express
npm install -g now
now
```

### Deploy with Glitch

Click the following button to edit on glitch

[![glitch-deploy-button](assets/deploy-glitch.png)](http://glitch.com/edit/#!/import/github/hasura/graphql-engine/community/boilerplates/auth-webhooks/nodejs-express)

## Usage with Hasura GraphQL engine

Once you have deployed this webhook, you can use it along with the GraphQL engine. You have to set the webhook URL as an environment variable in the docker container that runs the GraphQL engine.

*[Read the docs](https://hasura.io/docs/1.0/graphql/manual/auth/authentication/webhook.html).*
