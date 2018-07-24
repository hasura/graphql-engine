# Sample Firebase Clound Function Auth Webhook for Hasura GraphQL engine

## What is this?

We'll deploy Auth triggered Functions that send a welcome email when a new user signs up and a goodbye email when user accounts are deleted.

Further reading: [Firebase SDK for Cloud Functions](https://firebase.google.com/docs/functions/)


## How to install and deploy

 1. Create a Firebase Project using the [Firebase Console](https://console.firebase.google.com).
 1. Clone or download this repo and go to `cd firebase-cloud-functions` directory.
 1. You must have the Firebase CLI installed. If you don't have it install it with `npm install -g firebase-tools` and then configure it with `firebase login`.
 1. Configure the CLI locally by using `firebase init` and select your project in the list.
 1. [Add the Firebase Admin SDK to Your Server](https://firebase.google.com/docs/admin/setup) and save it as config.js
 1. Copy index.js and config.js to functions folder by: `cp index.js config.js functions/`
 1. Install Cloud Functions dependencies locally by running: `cd functions; npm install`
 1. Deploy to Firebase Cloud Functions by `firebase deploy`

 Once deployed you will get an endpoint like:

  ```bash
    https://us-central1-xxxxx-auth.cloudfunctions.net/hasuraWebhook
  ```
