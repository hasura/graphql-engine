# Sample Firebase Cloud Function Auth Webhook for Hasura GraphQL engine

Further reading: [Firebase SDK for Cloud Functions](https://firebase.google.com/docs/functions/)


## Install and deploy

 1. Create a Firebase Project using the [Firebase Console](https://console.firebase.google.com).
 1. Clone or download this repo and go to `cd firebase-cloud-functions` directory.
 1. You must have the Firebase CLI installed. If you don't have it install it with `npm install -g firebase-tools` and then configure it with `firebase login`.
 1. Configure the CLI locally by using `firebase init` and select your project in the list.
 1. Follow [Add the Firebase Admin SDK to Your Server](https://firebase.google.com/docs/admin/setup) and save it as config.js
 1. Copy index.js and config.js to functions folder by: `cp index.js config.js functions/`
 1. Install Cloud Functions dependencies locally by running: `cd functions; npm install`
 1. Deploy to Firebase Cloud Functions by `firebase deploy`

 Once deployed endpoint like this will be created and displayed:

  ```bash
    https://us-central1-xxxxx-auth.cloudfunctions.net/hasuraWebhook
  ```

## Add webhook endpoint to Hasura GraphQL

  Set `--auth-hook` or `HASURA_GRAPHQL_AUTH_HOOK` to the endpoint obtained above.

  [GraphQL engine server flags reference](https://docs.hasura.io/1.0/graphql/manual/deployment/graphql-engine-flags/reference.html) for details.

## Create table and set permission

  Follow [Common roles and auth examples](https://docs.hasura.io/1.0/graphql/manual/auth/authorization/common-roles-auth-examples.html)
  on Hasura doc for details of how to setup permission to a table.

  Make sure to change id column of user table to TXT type as uid sent from webhook is firebase User UID format (e.g. 0LnvZc7405TjRTbjURhZYYVXPI52)

## How to query GraphQL Engine from frontend JS code (React, VueJS, Angular etc...)

  postAxios.js
  ```bash
  import axiosBase from 'axios'
  import * as firebase from 'firebase'

  const getIdToken = async () => {
    return new Promise((resolve, reject) => {
      firebase.auth().onAuthStateChanged(function (user) {
        if (user) {
          resolve(firebase.auth().currentUser.getIdToken())
        } else {
          reject(Error('user logged out'))
        }
      })
    })
  }

  export const postAxios = async (queryString) => {
    const idToken = await getIdToken()

    const axios = axiosBase.create({
      baseURL: 'https://YOURHASURADOMAIN/v1/graphql',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + idToken
      },
      responseType: 'json',
      method: 'post'
    })

    return await axios({
      data: {
        query: queryString
      }
    }).catch(({response: r}) => console.log(r))
  }
  ```

  userService.js
  ```bash
  import { postAxios } from './postAxios'

  export default {
    async getUsers () {
      const queryString = `
        query {
          user
          {
            id
            name
          }
        }
    　`

      const result = await postAxios(queryString)
      return result.data.data.user
    }
  }
  ```
