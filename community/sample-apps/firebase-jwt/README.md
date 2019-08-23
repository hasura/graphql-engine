# Firebase Auth + Hasura JWT

Barebones example to show how to have Firebase Auth integrated with Hasura JWT mode.

## Firebase Auth

Firebase has few ways of implementing custom JWT claims in Firebase Auth:

1. Have firebase generate the JWTs, then customize them from your backend using
   Firebase Admin SDK [[docs]](https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_an_http_request)
2. Use Firebase cloud functions, and listen to user creation events to add
   custom claims to generated JWT [[docs]](https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_firebase_functions_on_user_creation)
3. Have your own backend server, which generates custom tokens [[docs]](https://firebase.google.com/docs/auth/admin/create-custom-tokens)
4. Have your own backend scripts (not initiated by the client) to update user custom claims [[docs]](https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_backend_script)

## Add custom claims in Firebase

In this example, we are choosing the option 2 from above. But this can be done via any of the above methods. [Firebase docs](https://firebase.google.com/docs/auth/admin/custom-claims) have extensive documentation on how to achieve this via different methods.

This example is adapted from [this guide](https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_firebase_functions_on_user_creation).

### Pre-requisites

This example assumes that you already have Firebase Auth setup for your app.

### Add the cloud function

Deploy the cloud function inside `functions/` folder:

```shell
firebase deploy --only functions
```

Customize the code to add your logic of assigning roles in the custom claims.

This cloud function is using the `onCreate` trigger. So whenever a user is created, this function is run.

### Client-side code

The client-side code is in `app/` folder.

## Configure Hasura to start in JWT mode

Deploy Hasura GraphQL Engine on Heroku:

[![Deploy HGE on heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

After deploying, add the following environment variables to configure JWT mode:

```
HASURA_GRAPHQL_ADMIN_SECRET : youradminsecretkey
```

```
HASURA_GRAPHQL_JWT_SECRET: {"type":"RS256", "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com", "audience": "<firebase-project-id>", "issuer": "https://securetoken.google.com/<firebase-project-id>"}
```

## Sending JWT to Hasura

Now, whenever you make a request to Hasura GraphQL engine (as an authenticated user), send the `id_token` in `Authorization` header:

```
Authorization: Bearer <firebase-id-token>
```
