.. _auth0_jwt:

Auth0 JWT Integration with Hasura GraphQL engine
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

In this guide, we will walk-through on how to set up Auth0 to work with Hasura GraphQL engine.

Create an Auth0 Application
^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Navigate to the `Auth0 dashboard <https://manage.auth0.com>`__
- Click on the ``Applications`` menu option on the left and then click the ``+ Create Application`` button.
- In the ``Create Application`` window, set a name for your application and select ``Single Page Web Applications``.
  (Assuming your application is React/Angular/Vue etc).

.. thumbnail:: ../../../../img/graphql/manual/guides/create-client-popup.png

Configure Auth0 Rules & Callback URLs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the settings of the application, add appropriate (e.g: http://localhost:3000/callback) URLs as ``Allowed Callback
URLs`` and ``Allowed Web Origins``. Add domain specific URLs as well for production apps. (e.g: https://myapp.com/callback)

In the dashboard, navigate to ``Rules``. Add the following rules to add our custom JWT claims:

.. code-block:: javascript

    function (user, context, callback) {
      const namespace = "https://hasura.io/jwt/claims";
      context.idToken[namespace] = 
        { 
          'x-hasura-default-role': 'user',
          // do some custom logic to decide allowed roles
          'x-hasura-allowed-roles': ['user'],
          'x-hasura-user-id': user.user_id
        };
      callback(null, user, context);
    }


.. _test-auth0:

Test auth0 login and generate sample JWTs for testing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You don't need to integrate your UI with auth0 for testing. You call follow the steps below:

1. Login to your auth0 app by heading to this URL: ``https://<auth0-domain>.auth0.com/login?client=<client_id>&protocol=oauth2&response_type=token%20id_token&redirect_uri=<callback_uri>&scope=openid%20profile``

   - Replace ``<auth0-domain>`` with your auth0 app domain.
   - Replace ``<client-id>`` with your auth0 app client id. Get your client id from app settings page on the auth0 dashboard.
   - Replace ``callback_uri`` with ``https://localhost:3000/callback`` or the URL you entered above. Note that this URL doesn't really need to exist while you are testing.

2. Once you head to this login page you should see the auth0 login page that you can login with.

.. image:: https://graphql-engine-cdn.hasura.io/img/auth0-login-page.png
   :class: no-shadow
   :alt: Auth0 login page

3. After successfully logging in, you will be redirected to ``https://localhost:3000/callback#xxxxxxxx&id_token=yyyyyyy``. This page may be a 404 if you don't have a UI running on localhost:3000.

.. image:: https://graphql-engine-cdn.hasura.io/img/auth0-localhost-callback-404.png
   :class: no-shadow
   :alt: Auth0 successful callback 404 page

4. Extract the ``id_token`` value from this URL. This is the JWT.

.. image:: https://graphql-engine-cdn.hasura.io/img/id_token-jwt-url.png
   :class: no-shadow
   :alt: JWT from id_token query param

5. To test this JWT, and to see if all the Hasura claims are added as per the sections above, lets test this out with `jwt.io <https://jwt.io>`__!

.. image:: https://graphql-engine-cdn.hasura.io/img/jwt-io-debug.png
   :class: no-shadow
   :alt: JWT debug on jwt.io

**Save this JWT token value so that we can use it later to test authorization using the Hasura console.**

**Note**: In case the above method gives an error, try disabling OIDC Conformant setting (https://auth0.com/docs/api-auth/tutorials/adoption/oidc-conformant) under Advanced Settings -> OAuth.

Configure Hasura to use Auth0 Keys
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Auth0 publishes their JWK under:

``https://<your-auth0-domain>.auth0.com/.well-known/jwks.json``

But they have a `bug where the certificate thumbprint does not match
<https://community.auth0.com/t/certificate-thumbprint-is-longer-than-20-bytes/7794/3>`__.
Hence, currently this URL does not work with Hasura.

Current workaround is to download the X590 certificate from:

``https://<your-auth0-domain>.auth0.com/pem``

And use it in the ``key`` field:

.. code-block:: json

        {
          "type":"RS512",
          "key": "-----BEGIN CERTIFICATE-----
    MIIDDTCAfWgAwIBAgIJhNlZ11IDrxbMA0GCSqSIb3DQEBCwUAMCQxIjAgBgNV
    BAMTGXlc3QtaGdlLWp3C5ldS5hdXRoMC5jb20HhcNMTgwNzMwMTM1MjM1WhcN
    MzIwND3MTM1MjM1WjAkSIwIAYDVQQDExl0ZXNLWhnZS1qd3QuZXUuYXV0aDAu
    Y29tMIBIjANBgkqhkiGw0BAQEFAAOCAQ8AMIICgKCAQEA13CivdSkNzRnOnR5
    ZNiReD+AgbL7BWjRiw3RwjxRp5PYzvAGuj94yR6LRh3QybYtsMFbSg5J7fNq6
    Ld6yMpMrUu8CBOnYY456b/2jlf+Vp8vEQuKvPOOw8Ev6x7X3blcuXCELSwyL3
    AGHq9OP2RV6V6CIE863zzuYH5HDLzU35oMZqogJVRJM0+6besH6TnSTNiA7xi
    BAqFaiRNQRVi1CAUa0bkN1XRp4AFy7d63VldOsM+8QnCNHySdDr1XevVuq6DK
    LQyGexFy4niALgHV0Q7A+xP1c2G6rJomZmn4j1avnlBpU87E58JMrRHOCj+5m
    Xj22/QDAQABo0IwQDAPgNVHRMBAf8EBTADAQHMB0GA1UdDgQWBBT6FvNkuUgu
    tk3OYQi4lo5aOgwazAOgNVHQ8BAf8EBAMCAoQDQYJKoZIhvcNAQELBQADggEB
    ADCLj+L22pEKyqaIUlhUJh7DAiDSLafy0fw56CntzPhqiZVVRlhxeAKidkCLV
    r9IEbRuxUoXiQSezPqM//9xHegMp0f2VauVCFg7EpUanYwvqFqjy9LWgH+SBz
    4uroLSZ5g1EPsHtlArLChA90caTX4e7Z7Xlu8G2kHRJB5nC7ycdbMUvEWBMeI
    tn/pcbmZ3/vlgj4UTEnURe2UPmSJpxmPwXqBcvwdKHRMgFXhZxojWCi0z4ftf
    f8t8UJIcbEblnkYe7wzYy8tOXoMMHqGSisCdkp/866029rJsKbwd8rVIyKNC5
    frGYaw+0cxO6/WvSir0eA=
    -----END CERTIFICATE-----
    "
        }

An easier way to generate the above config is to use the following UI:

https://hasura.io/jwt-config.

The generated config can be used in env ``HASURA_GRAPHQL_JWT_SECRET`` or ``--jwt-secret`` flag.
The config generated from this page can be directly pasted in yaml files and command line arguments as it takes care of
escaping new lines.

.. thumbnail:: ../../../../img/graphql/manual/auth/jwt-config-generated.png
   :width: 75%


Add Access Control Rules via Hasura Console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Auth0 is configured and ready to be used in the application. You can now setup access control rules that
will automatically get applied whenever a client makes a graphql query with the Auth0 token.

Refer :doc:`../../auth/authorization/basics` for more information.

To test this out, add an access control rule that uses ``x-hasura-user-id`` for the role ``user``.
Then make a GraphQL query or a mutation, with the Authorization token from the :ref:`previous step <test-auth0>`
where we generated an Auth0 token.

.. image:: https://graphql-engine-cdn.hasura.io/img/jwt-header-auth-hasura.png
   :class: no-shadow
   :alt: JWT token used as bearer token on hasura console

You can also use the env variable ``HASURA_GRAPHQL_UNAUTHORIZED_ROLE`` or ``--unauthorized-role`` flag to set a role
for **unauthorized users** (e.g. ``anonymous``). This will allow you to set permissions for users that are not
logged in.

The configured unauthorized role will be used whenever an access token is not present in a request to the GraphQL API. 

This can be useful for data that you would like anyone to be able to access and can be configured and restricted
just like any other role.
