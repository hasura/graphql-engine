.. meta::
   :description: Frequently asked questions about Hasura's Auth service.
   :keywords: hasura, docs, auth, FAQs, FAQ, common questions

FAQ
===

Why is the user role given by default?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Because, this role can be then leveraged by any service (running inside the
project) to provide authorization and access control mechanisms very easily.

Like, Hasura Auth APIs use this role to allow access to profile management APIs
of a user. (Profile management APIs are any API starting with ``/user``. Like
email change, password reset etc. APIs.)

To illustrate, the role information is passed in the header by the API gateway
when the request reaches the upstream service. This service can then look at
the headers and perform its own authorization and access control easily.

What if the user role is removed from a user?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The user won't be able to access any profile management(account info, password
change, password reset, email change etc. - basically any API behind
``/user/``) APIs, if the ``user`` role is removed from a user.

What if the user role itself is removed?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You cannot remove the ``user`` role, it is default to the system.

What if a new role needs to be assigned to a user?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can create custom roles, and use the admin APIs of Hasura Auth to assign
roles to a user.

You can also use the project console to assign roles to a user.

How is a user verified during registration?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
User verification is switched off by default.  There are few verification
methods that are built-in to the system, like email verification, mobile
verification and Recaptcha, which you can enable and configure from the project
console.

If you need any other kind of custom verification, you can write your own
custom logic and endpoint for user registration. And your APIs can internally
use the Hasura Auth admin APIs to create and manage users and their roles.
