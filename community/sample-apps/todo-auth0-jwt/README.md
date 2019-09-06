# Integrating Todo app with Auth0 and JWT authorization with Hasura GraphQL Engine

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

**NOTE:** You can go to https://hasura.io/jwt-config and generate the config easily (and skip the following steps).

Download your JWT signing X509 certificate by visiting URL:
`https://<YOUR-AUTH0-DOMAIN>/pem`

Convert the file into one-line, this will be required later:

```shell
awk 'NF {sub(/\r/, ""); printf "%s\\n",$0;}' yourauth0subdomain.pem
```

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

## Configure the application

Setup values in `todo-app/src/constants.js`:
1. Auth0 domain
2. GraphQL engine deployed URL, e.g: `https://hasura-todo-auth0-jwt.herokuapp.com/v1/graphql`
3. Auth0 application's client id

## Create the initial tables
1. Add your database URL and admin secret in `hasura/config.yaml`

```yaml
endpoint: https://<hge-heroku-url>
admin_secret: <your-admin-secret>
```

2. Run `hasura migrate apply` to create the required tables and permissions for the todo app

## Run the application

`$ npm install && npm start`

  > The app runs on port 3000 by default. You can change the port number, but you will also have to reconfigure the callback


## Code
- All the Auth0 related code is in `todo-app/src/Auth`
- In `todo-app/src/routes.js`, we get the `id_token` from localstorage, and send
  as `Authorization` header to HGE.
