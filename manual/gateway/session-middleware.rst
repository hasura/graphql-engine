API gateway as Session Middleware
=================================

The API gateway has a session middleware built into it. This means, for all
external requests (i.e requests coming from outside the cluster) the gateway
looks for a ``Cookie`` or an ``Authorization`` header, resolves a session based on
that information, and proxies the request to the correct upstream microservice
with the session information in special HTTP headers. Specifically, these
headers are ``X-Hasura-User-Id``, ``X-Hasura-Role`` and
``X-Hasura-Allowed-Roles``.

So an upstream microservice doesn't have to implement its own logic of
resolving session from the request. It can just read the special headers
forwarded by the gateway to determine which user made that request, what is the
role of the user etc. Based on this information it can then implement its own
authorization logic.

By default the gateway sets the ``X-Hasura-Role`` value to the first role assigned to a user.
If a request is made with a specific ``X-Hasura-Role`` header, the gateway will pass that value
to the upstream microservice if the user has that particular role or else will reject the request
with a ``403 Forbidden`` response.

For logged in users
~~~~~~~~~~~~~~~~~~~
If the gateway could resolve a valid session from the cookie or
``Authorization`` header, then values of the ``X-Hasura-*`` headers will be:

* ``X-Hasura-User-Id`` : ``hasura_id`` of the user (e.g ``X-Hasura-User-Id:
  42``).
* ``X-Hasura-Role`` : Current role of the logged in user. (e.g ``X-Hasura-Role:
  user``).
* ``X-Hasura-Allowed-Roles`` : Comma separated values of all roles that user
  has (e.g ``X-Hasura-Allowed-Roles: user,admin``).


For non-logged in users
~~~~~~~~~~~~~~~~~~~~~~~
If the request does not contain any cookie or ``Authorization`` header, the
gateway will add anonymous values in the ``X-Hasura-*`` headers.

* ``X-Hasura-User-Id`` : Value will be ``0``.
* ``X-Hasura-Role`` : Value will be ``anonymous``.