# Sample Auth Webhook for Hasura GraphQL engine

This is a sample auth webhook for authenticating requests to the Hasura GraphQL engine.

It has boilerplate code written for firebase auth. There is also a generic sample handler in `server.js` where you can handle your custom auth providers.

## Quick deploy

<!--
### Deploy with Heroku (recommended)

1. Click the following button for deploying to Heroku.

   [![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/sample-auth-webhook)

2. Once it is deployed, go to `Manage App > Settings` of your app and set the following environment variables if you want to use Firebase.

   - **FIREBASE_CONFIG**: Copy the contents of your serviceAccount JSON file for this field. Example:
     ```
        {
          "type": "service_account",
          "project_id": "testapp-2222",
          "private_key_id": "f02aca08952f702de43ed577b428f405efe2d377",
          "private_key": "-----BEGIN PRIVATE KEY-----\n<your-private-key>\n-----END PRIVATE KEY-----\n",
          "client_email": "firebase-adminsdk-t4sik@testapp-24a60.iam.gserviceaccount.com",
          "client_id": "113608616484852272199",
          "auth_uri": "https://accounts.google.com/o/oauth2/auth",
          "token_uri": "https://accounts.google.com/o/oauth2/token",
          "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
          "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-t4sik%40testapp-22222.iam.gserviceaccount.com"
        }
     ```

   If you are not using an auth provider, you need not enter the environment variable associated with it
-->

### Deploy using [Now](https://zeit.co/now)

Run the following commands to deploy using Now.

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/community/boilerplates/auth-webhooks/nodejs-express
npm install -g now
now -e \
FIREBASE_CONFIG='{
        "type": "service_account",
        "project_id": "testapp-2222",
        "private_key_id": "f02aca08952f702de43ed577b428f405efe2d377",
        "private_key": "-----BEGIN PRIVATE KEY-----\n<your-private-key>\n-----END PRIVATE KEY-----\n",
        "client_email": "firebase-adminsdk-t4sik@testapp-24a60.iam.gserviceaccount.com",
        "client_id": "113608616484852272199",
        "auth_uri": "https://accounts.google.com/o/oauth2/auth",
        "token_uri": "https://accounts.google.com/o/oauth2/token",
        "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
        "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-t4sik%40testapp-22222.iam.gserviceaccount.com"
      }'
```

If you are not using an auth provider, you need not enter the environment variable associated with it. For example, if you are not using firebase, the last command you should run is,

```bash
$ now
```

### Deploy with Glitch

1. Click the following button to edit on glitch

   [![glitch-deploy-button](assets/deploy-glitch.png)](http://glitch.com/edit/#!/import/github/hasura/graphql-engine/community/boilerplates/auth-webhooks/nodejs-express)

2. Add the following environment variables in the `.env` file on glitch.

   ```env
   FIREBASE_CONFIG='{
     "type": "service_account",
     "project_id": "testapp-2222",
     "private_key_id": "f02aca08952f702de43ed577b428f405efe2d377",
     "private_key": "-----BEGIN PRIVATE KEY-----\n<your-private-key>\n-----END PRIVATE KEY-----\n",
     "client_email": "firebase-adminsdk-t4sik@testapp-24a60.iam.gserviceaccount.com",
     "client_id": "113608616484852272199",
     "auth_uri": "https://accounts.google.com/o/oauth2/auth",
     "token_uri": "https://accounts.google.com/o/oauth2/token",
     "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
     "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-t4sik%40testapp-22222.iam.gserviceaccount.com"
   }'
   ```

   If you are not using an auth provider, you need not enter the environment variable associated with it.


## Usage with Hasura GraphQL engine

Once you have deployed this webhook, you can use it along with the GraphQL engine. You have to set the webhook URL as an environment variable in the docker container that runs the GraphQL engine.

*[Read the docs](https://docs.hasura.io/1.0/graphql/manual/auth/authentication/webhook.html).*

### Firebase

Send the firebase `id_token` as a header while making queries to the `graphql-engine`.

```
{
  "Authorization": "Bearer <id_token>"
}
```
