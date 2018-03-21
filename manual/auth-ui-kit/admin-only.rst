.. .. meta::
   :description: Using Hasura Auth UI Kit for admin only restricted page
   :keywords: hasura, users, auth, uikit, restricted


.. _uikit-admin-usecase:

Auth UI Kit: Restrict to Admin Only
===================================


There are cases where you would like to deploy some microservices which are accessible only by admin or a particular set of people (based on authentication roles). 

In those cases, if you have setup Authorisation Policy with restrictToRoles along with 

1. ``noAccessRedirectUrl``

If this is setup, the gateway will redirect to the UI Kit's restricted page asking the user to logout and login as a user with a role that is allowed to access the restricted page.

2. ``noSessionRedirectUrl``

If this is setup, the gateway will redirect to the UI Kit's login page, because no session is present.

For example, let's say you have an admin panel running at ``admin`` subdomain of your cluster. i.e ``https://admin.<cluster-name>.hasura-app.io``. You would only want users with admin role to be able to see this page. In ``conf/routes.yaml`` of your hasura project, your conf would look something like this.

.. code-block:: yaml

  admin:
    /:
      upstreamService:
        name: admin
        namespace: {{ cluster.metadata.namespaces.user }}
      upstreamServicePath: /
      upstreamServicePort: 80
      authorizationPolicy:
        restrictToRoles: ["admin"]
        noSessionRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/
        noAccessRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/restricted


Note the usage of ``noSessionRedirectUrl`` and ``noAccessRedirectUrl``.
