# React-Hasura-Boilerplate - advanced

## Getting Started

### Installing

You need to first install all the npm packages that are used in this boilerplate.

```
$ npm install
```

Once all the dependencies are installed, you're ready to go!

```
$ npm start
```
This starts the development server at port 3000 (localhost).


### What does this boilerplate contain ?

This boilerplate contains a template applications with auth0 integrated for user authentication.

( download the [sample application using react](https://manage.auth0.com/#/applications/CA6e7EkF1oMRvWSo0et1exQT9PDT7nzc/quickstart) and try to integrate it in your application, this is the quickest way to integrate auth0 into your application )

##### In your `/Auth` folder, we create a file `Auth.js` where we instantiate an `auth0` variable we get from the package `auth0-js`.
#
#
```
import auth0 from 'auth0-js';
import { AUTH_CONFIG } from './auth0-variables';

export default class Auth {
    auth0 = new auth0.WebAuth({
        domain: AUTH_CONFIG.domain,
        clientID: AUTH_CONFIG.clientId,
        redirectUri: AUTH_CONFIG.callbackUrl,
        responseType: 'token id_token',
        scope: 'openid'
    });
    ...
}
```  
This class `Auth` is then used to initialize a vairable `auth` in your `routes.js` file, where we set the Component to be rendered when routing passing this variable as a prop in those Component.

```
<Route path="/" render={(props) => <App auth={auth} {...props} />} />
```
##### How the webhook works ?
#
```
var express = require('express');
var auth0Router = express.Router();
var requestClient = require('request');
var auth0Domain = process.env.AUTH_ZERO_DOMAIN;

auth0Router.route('/webhook').get((request, response) => {
  // Throw 500 if auth0 domain is not configured
  if (!auth0Domain) {
    response.status(500).send('Auth0 domain not configured');
    return;
  }

  var token = request.get('Authorization');

  if (!token) {
    response.json({'x-hasura-role': 'anonymous'});
    return;
  } else {
    // Fetch information about this user from
    // auth0 to validate this token
    // NOTE: Replace the URL with your own auth0 app url
    var options = {
      url: `https://${auth0Domain}/userinfo`,
      headers: {
        Authorization: token,
        'Content-Type': 'application/json'
      }
    };

    requestClient(options, (err, res, body) => {
      if (!err && res.statusCode == 200) {
        var userInfo = JSON.parse(body);
        console.log(userInfo); //debug
        var hasuraVariables = {
          'X-Hasura-User-Id': userInfo.sub,
          'X-Hasura-Role': 'user'
        };
        console.log(hasuraVariables); // For debug
        response.json(hasuraVariables);
      } else {
        // Error response from auth0
        console.log(err, res, body);
        response.json({'x-hasura-role': 'anonymous'});
        return;
      }
    });
  }
});
```
Refer to this [link](https://github.com/hasura/sample-auth-webhook) to see exaclty how this works.

On successful connection with the webhook, it sends user information to the engine directly as a header.

```
var hasuraVariables = {
    'X-Hasura-User-Id': userInfo.sub,
    'X-Hasura-Role': 'user'
};
```

#### Using the `sub` property :
#
The `sub` property is set when authenticating the user by auth0, it is set in the `localStorage`

##### property  is `set` in __Auth.js__ :
#
```
localStorage.setItem('sub', authResult.idTokenPayload.sub);
```
##### property is `get` in __/Components/AddTodo.js__
#
```
const sub = localStorage.getItem('sub');
```
This can be passed into the mutation and stored in the table saying this todo belongs to this particular user.

### Deployment

First create a file `env.js` in your `/src` folder, this file will contain your environment variables needed for the app to work.

```
export const vars = {
  "GRAPHQL_ENDPOINT": "https://",
  "DOMAIN": "", //app.autho.com
  "CLIENT_ID": "",
  "CALLBACK_URL": "https://localhost:3000/callback"
}
```
#### Hasura console configs :
- Set permissions for a new role `user`.
- modify the access control for `insert`, `update`, `delete` and `select` accordingly, refer to this [link](https://docs.hasura.io/1.0/graphql/manual/auth/basics.html) to see how it needs to be done.
- ![permissions](/advanced/ss/permissions.png)
#### Auth0 configs :
- Go to `Applications` on your auth0 dashboard, set the environment variables accordingly.
- In the callback url field, use only the url that you have specified in the `Allowed Callback URLs` field in your auth0 dashbaord.
- In the field `Allowed Origins (CORS)` in your auth0 dashboard, you may add 2 urls, `<your- app-url>` and `<your-callback-url>` just incase you get `CORS` error.

#### Heroku configs :
- Go to app `Settings`, open `Reveal Config Vars`, you need to set a few variables mentioned below.
    `HASURA_GRAPHQL_CORS_DOMAIN` - add your app URL.
    `HASURA_GRAPHQL_AUTH_HOOK` - URL of your webhook.
- For other [GraphQL server options](https://docs.hasura.io/1.0/graphql/manual/deployment/graphql-engine-flags/reference.html).
- Your may deploy your webhhook anywhere you wish to __heroku__ or __glitch__.

To setup your own webhook refer to this [link](https://github.com/hasura/sample-auth-webhook).
