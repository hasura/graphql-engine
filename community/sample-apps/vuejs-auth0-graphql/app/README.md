# vuejs-auth0-graphql

This sample Vue.js app demonstrates:

- Logging in to Auth0 using Redirect Mode
- Making an authenticated graphql query fetching articles written by the logged in user
- Accessing profile information that has been provided in the ID token
- Gated content. The `/profile` route is not accessible without having first logged in

## Integrating Vue App with Auth0 and JWT authorization with Hasura GraphQL Engine

In this example, we use Hasura GraphQL engine's JWT authorization mode. We use
Auth0 as our authentication and JWT token provider.

## Create an application in Auth0

1. Create an application in Auth0 dashboard

2. In the settings of the application, add `http://localhost:3000/callback` as
   "Allowed Callback URLs" and `http://localhost:3000` as "Allowed Web Origins"

## Add rules for custom JWT claims

In the Auth0 dashboard, navigate to "Rules". Add the following rules to add our custom JWT claims:

```javascript
function (user, context, callback) {
  const namespace = "https://hasura.io/jwt/claims";
  context.idToken[namespace] = 
    { 
      'x-hasura-default-role': 'user',
      // do some custom logic to decide allowed roles
      'x-hasura-allowed-roles': user.email === 'admin@foobar.com' ? ['user', 'admin'] : ['user'],
      'x-hasura-user-id': user.user_id
    };
  callback(null, user, context);
}
```

## Get your JWT signing certificate

Head to [https://hasura.io/jwt-config](https://hasura.io/jwt-config) and generate the config for your auth0 domain.

## Deploy Hasura GraphQL Engine

[![Deploy HGE on heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

After deploying, add the following environment variables to configure JWT mode:

```
HASURA_GRAPHQL_ADMIN_SECRET: youradminsecretkey
```

```
HASURA_GRAPHQL_JWT_SECRET: {"type":"RS256", "key": "<the-certificate-data-in-one-line>"}
```

For example, (copy the certificate from above step or use generated config from https://hasura.io/jwt-config):

```
HASURA_GRAPHQL_JWT_SECRET: {"type":"RS256", "key": "-----BEGIN CERTIFICATE-----\nMIIDDTCCAfWgAwIBAgIJPhNlZ11IDrxbMA0GCSqGSIb3DQEBCQxIjAgNV\nBAMTGXRlc3QtaGdlLWp3dC5ldS5hdXRoMC5jb20wHhcNMTgwNzMwMTM1MjM1WhcN\nMzIwNDA3MTM1MjM1WjAkMSIwIAYDVQQDExl0ZXN0LWhnZS1qd3QuZXUuYXV0aDAu\nY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA13CivdSkNzRnOnR5iReDb+AgbL7BWjRiw3tRwjxRp5PYzvAGuj94y+R6LRh3QybYtsMFbSg5J7fNq6\nLd6yMpRMrUu8CBOnYY45D6b/2jlf+Vp8vEQuKvPMOOw8Ev6x7X3blcuXCELSwyL3\nAGHq9OpP2RV6V6CIE863IzzuYH5HDLzU35oMZqozgJVRJM0+6besH6TnSTNiA7xi\nBAqFaiQRNQRVi1CAUa0bLkN1XRp4AFy7d63VldO9sM+8QnCNHySdDr1XevVuq6DK\nLQyGexFFy4niALgHV0Q7QA+xP1c2G6rJomZmn4jl1avnlBpU87E58JMrRHOCj+5m\nXj22AQABo0IwQDAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBT6FvNkuUgu\YQ/i4lo5aOgwazAOBgNVHQ8BAf8EBAMCAoQwDQYJKoZIhvcNAQELBQADggEB\nADCLj+/L22pEKyqaIUlhHUJh7DAiDSLafy0fw56UCntzPhqiZVVRlhxeAKidkCLVIEbRLuxUoXiQSezPqMp//9xHegMp0f2VauVCFbg7EpUanYwvqFqjy9LWgH+SBz\n4uroLSYZ5g1EPsHtlArLRChA90caTX4e7Z7Xlu8vG2kHRJB5nC7ycdbMUvEWBMeI\ntn/pcb4mZ3/vlgj4UTEnCURe2UPmSJpxmPwXqBctvwdKHRMgFXhZxojWCi0z4ftf\nf8t8UJSIcbEblnkYe7wzRYy8tOXoMMHqGSisCdkWp/866029rJsKbwd8rVIyKNC5\nfrGYawv+0cxO6/Sir0meA=\n-----END CERTIFICATE-----"}
```

Save changes.

## Configure the Auth0 Application

The project needs to be configured with your Auth0 domain and client ID in order for the authentication flow to work.

To do this, open `auth_config.json`, and replace the values within with your own Auth0 application credentials:

```json
{
  "domain": "<YOUR AUTH0 DOMAIN>",
  "clientId": "<YOUR AUTH0 CLIENT ID>"
}
```

## Create the initial tables
1. Add your database URL and admin secret in `hasura/config.yaml`

```yaml
endpoint: https://<hge-heroku-url>
admin_secret: <your-admin-secret>
```

2. Run `hasura migrate apply` inside `hasura` directory to create the required tables and permissions for the app

## Create Auth0 Rule

Everytime user signups on Auth0, we need to sync that user into our postgres database. This is done using Auth0 rules. Create a Rule and insert the following code:

```
function (user, context, callback) {
  const userId = user.user_id;
  const nickname = user.nickname;
  
  request.post({
  headers: {'content-type' : 'application/json', 'x-hasura-admin-secret': '<your-admin-secret>'},
  url:     'http://myapp.herokuapp.com/v1/graphql',
  body:    `{\"query\":\"mutation($userId: String!, $nickname: String) {\\n          insert_users(\\n            objects: [{ auth0_id: $userId, name: $nickname }]\\n            on_conflict: {\\n              constraint: users_pkey\\n              update_columns: [last_seen, name]\\n            }\\n          ) {\\n            affected_rows\\n          }\\n        }\",\"variables\":{\"userId\":\"${userId}\",\"nickname\":\"${nickname}\"}}`
}, function(error, response, body){
    console.log(body);
    callback(null, user, context);
});
}
```

## Run the application

`npm install && npm run serve`

  > The app runs on port 3000 by default. You can change the port number, but you will also have to reconfigure the callback

