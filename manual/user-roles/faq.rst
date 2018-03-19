User roles: FAQ
===============

Why is the user role given by default?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Because, this role can be then leveraged by any microservice (running inside the
project) to provide authorization and access control mechanisms very easily.

Like, Hasura Auth APIs use this role to allow access to profile management APIs
of a user. (Profile management APIs are any API starting with ``/user``. Like
email change, password reset etc. APIs.)

To illustrate, the role information is passed in the header by the API gateway
when the request reaches the upstream microservice. This microservice can then look at
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

You can also use the api console to assign roles to a user.
