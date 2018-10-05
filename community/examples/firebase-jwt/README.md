# Firebase Auth + Hasura JWT

Barebones example to show how to have Firebase Auth integrated with Hasura JWT mode.

# Firebase Auth

Firebase has few ways of implementing custom JWT claims in Firebase Auth:

1. Have firebase generate the JWTs, then customize them from your backend using
   Firebase Admin SDK. (https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_an_http_request)
2. Use Firebase cloud functions, and listen to user creation events to add
   custom claims to generated JWT. (https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_firebase_functions_on_user_creation)
3. Have your own backend server, which generates custom tokens. (https://firebase.google.com/docs/auth/admin/create-custom-tokens)
4. Have your own backend scripts (not initiated by the client) to update user custom claims (https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_backend_script)

# Add custom claims in Firebase

In this example, we are choosing the option 2 from above. But this can be done via any of the above methods. [Firebase docs](https://firebase.google.com/docs/auth/admin/custom-claims) have extensive documentation on how to achive this via different methods.

This example is adapted from https://firebase.google.com/docs/auth/admin/custom-claims#defining_roles_via_firebase_functions_on_user_creation

## Pre-requisites

This example assumes that you already have Firebase Auth setup for your app.

## Add the cloud function

1. Create a cloud function via the Firebase CLI (https://firebase.google.com/docs/functions/)
2. Add the following code to your cloud function:

```javascript
const functions = require('firebase-functions');

const admin = require('firebase-admin');
admin.initializeApp(functions.config().firebase);

// On sign up.
exports.processSignUp = functions.auth.user().onCreate(event => {
  const user = event.data; // The Firebase user.
  // Check if user meets role criteria:
  // have custom logic to decide what roles should the user get
  let customClaims;
  if (user.email &&
      user.email.indexOf('@admin.example.com') != -1 &&
      user.emailVerified) {
    customClaims = {
      'https://hasura.io/jwt/claims': {
        'x-hasura-default-role': 'admin',
        'x-hasura-allowed-roles': ['user', 'admin']
      }
    };
  }
  else {
    customClaims = {
      'https://hasura.io/jwt/claims': {
        'x-hasura-default-role': 'user',
        'x-hasura-allowed-roles': ['user']
      }
    };
  }
  // Set custom user claims on this newly created user.
  return admin.auth().setCustomUserClaims(user.uid, customClaims)
    .then(() => {
      // Update real-time database to notify client to force refresh.
      const metadataRef = admin.database().ref("metadata/" + user.uid);
      // Set the refresh time to the current UTC timestamp.
      // This will be captured on the client to force a token refresh.
      return metadataRef.set({refreshTime: new Date().getTime()});
    })
    .catch(error => {
      console.log(error);
    });
});

```

Customize the code to add your logic of assigning roles in the custom claims.

This cloud function is using the `onCreate` trigger. So whenever a user is created, this function is run.

## Client-side code

Your client-side code should be something like:

```javascript
const provider = new firebase.auth.GoogleAuthProvider();
firebase.auth().signInWithPopup(provider)
.catch(error => {
  console.log(error);
});

let callback = null;
let metadataRef = null;
firebase.auth().onAuthStateChanged(user => {
  // Remove previous listener.
  if (callback) {
    metadataRef.off('value', callback);
  }
  // On user login add new listener.
  if (user) {
    // Check if refresh is required.
    metadataRef = firebase.database().ref('metadata/' + user.uid + '/refreshTime');
    callback = (snapshot) => {
      // Force refresh to pick up the latest custom claims changes.
      // Note this is always triggered on first call. Further optimization could be
      // added to avoid the initial trigger when the token is issued and already contains
      // the latest claims.
      user.getIdToken(true);
    };
    // Subscribe new listener to changes on that node.
    metadataRef.on('value', callback);
  }
});
```

# Configure Hasura to start in JWT mode

Deploy Hasura GraphQL Engine on Heroku:

[![Deploy HGE on heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

After deploying, add the following environment variables to configure JWT mode:

```
HASURA_GRAPHQL_ACCESS_KEY : yoursecretaccesskey
```

```
HASURA_GRAPHQL_JWT_SECRET: {"type":"RS512", "jwk_url": "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"}
```

# Sending JWT to Hasura

Now, whenever you make a request to Hasura GraphQL engine (as an authenticated user), send the `id_token` in `Authorization` header:

`Authorization: Bearer <firebase-id-token>`
