.. meta::
   :description: Reference documentation for hasura-project-conf, a single JSON object to encapsulate a Hasura project, its Kubernetes structure, JSON object schema.
   :keywords: hasura, docs, project configuration, proj conf, configuration, hasura-project-conf

.. _hasura-project-conf:

Hasura project configuration - ``hasura-project-conf``
======================================================

This is a JSON object that contains the full configuration of a hasura project. Since some
configuration values cannot be stored raw in version control, the configuration object can
refer to the ``hasura-project-secrets`` object for its values. The project configuration
is made available to the platform as a kubernetes ``configmap``.

.. note:: To fetch the current desired project configuration of the cluster use the
  following kubectl command:

  ``kubectl get cm hasura-project-conf``

Kubernetes structure
--------------------

``hasura-project-conf`` is a kubernetes ``configmap``.
Here's a sample condensed yaml file that represents a valid ``hasura-project-conf`` resource.

.. code-block:: yaml

   apiVersion: v1
   data:
     project: |
       {
         "services": {...},
         "gateway": {...},
         "ssh": {...},
         "continuousIntegration": {...}
       }
   kind: ConfigMap
   metadata:
     name: hasura-project-conf
     namespace: default


JSON Object schema
------------------

The specification of the JSON object in the ``data.project`` field of the ``configmap`` is
described below:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - version
     - true
     - String
     - The version of the project configuration being used
   * - services
     - true
     - :ref:`ServicesConf <services-conf>`
     - An object that with the services names as keys, storing per-service configuration
   * - gateway
     - true
     - :ref:`GatewayConf <gateway-conf>`
     - An object to configure domains, routes & nginx http-directives
   * - ssh
     - true
     - :ref:`SshConf <ssh-conf>`
     - An object that with the services names as keys, storing per-service configuration
   * - continuousIntegration
     - true
     - :ref:`CIConf <ci-conf>`
     - An object that with the services names as keys, storing per-service configuration

.. _services-conf:

ServicesConf
^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - postgres
     - true
     - :ref:`PostgresConf <postgres-conf>`
     - Postgres configuration variables (important for initialising postgres with a username/password)
   * - auth
     - true
     - :ref:`AuthConf <auth-conf>`
     - An object that with the services names as keys, storing per-service configuration


.. _postgres-conf:

PostgresConf
^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - database
     - true
     - string
     - database to be used by the Hasura Data API (default: ``hasuradb``)
   * - user
     - true
     - string
     - Postgres user with read/write access to the database (default: ``admin``)
   * - password
     - true
     - string
     - Password for the postgres user
   * - port
     - true
     - string
     - Port that the postgres service is available on (default: ``5432``)

.. _auth-conf:

AuthConf
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - djangoSecretKey
     - true
     - string
     - Randomly generated key used for signing cookies
   * - debug
     - true
     - string
     - Set to ``""`` for false, to ``"true"`` to enable Django's debug mode (default: ``""``)
   * - logLevel
     - true
     - string
     - DEBUG or INFO or WARN or ERROR (default: ``ERROR``)
   * - saUser
     - true
     - string
     - The username of the default admin created during project initialisation (default: ``admin``)
   * - saPassword
     - true
     - string
     - The password of the default admin created during project initialisation
   * - saEmail
     - true
     - string
     - The email of the default admin created during project initialisation
   * - saMobile
     - true
     - string
     - The mobile of the default admin created during project initialisation
   * - logoutMethods
     - true
     - string
     - ``'["POST"]'`` by default to enable logouts using POST only (see: `stackoverflow.com/a/1458... <http://stackoverflow.com/a/14587231/3364697>`_). Use ``'["GET", "POST"]`` for backwards compatibility
   * - passwordMinLength
     - true
     - string
     - Stringified integer representing minimum password length required for registration (default: ``"8"``)
   * - passwordResetExpiresDays
     - true
     - string
     - Stringified integer representing the number of days the password reset link is valid for (default: ``"2"``)
   * - allowUserAccountDelete
     - true
     - string
     - ``"yes"`` if you want logged-in users to be able to delete themselves from the auth service; ``""`` otherwise (default: ``""``)
   * - cookieName
     - true
     - string
     - Cookie name used for storing session tokens (default: ``dinoisses``)
   * - cookieAge
     - true
     - string
     - Stringified integer for number of secods that cookies are active for (default: ``1209600``)
   * - recaptchaEnabled
     - true
     - string
     - ``""`` for no recaptcha, ``"yes"`` to enable recatpcha during registration (default: ``""``)
..    * - recaptchaSecret:
..      - tr
..      # email
..      emailVerificationMandatory: "" # set empty string if want to set to false; default: false
..      updateUserOnEmailConfirm: "yes" # set empty string if want to set to false; default: true
..      emailProvider: "sparkpost" # valid: 'sparkpost', 'aws', 'mandrill'
..      emailSenderId: "admin@example.com" # email address of the sender
..      emailSenderName: "Example Webmaster" # name of the sender
..      emailSparkpostKey:
..      secretKeyRef:
..      name: hasura-project-secrets
..      key: auth.sparkpost.key
..      emailMandrillKey: ""
..      emailAwsKey: ""
..      emailAwsSecretKey: ""
..      emailVerificationExpiresDays: "5" # default: 5
..      # templates uses Python's string interpolation. Remember to escape %
..      # characters in your templates.
..      emailTemplateRegister: |
..      Hi, Please click on http://myawesomeapp.com/%(token)s to verify your email.
..      emailTemplateVerify: |
..      Hi, Please click on http://myawesomeapp.com/%(token)s to verify your email.
..      emailTemplateForgotPass: |
..      Hi %(name)s, <br/>click on http://myawesomeapp.com/resetpassword/%(token)s to reset your password.
..      emailTemplateWelcomeNew: ""
..      emailTemplateEmailChanged: ""
..      emailTemplateMobileChanged: ""
..      # mobile
..      mobileVerificationMandatory: "" # set empty string if want to set to false; default: false
..      smsProvider: msg91 # valid: msg91
..      mobileVerificationExpiresMins: "15" # in mins. default: 15
..      mobileMsg91Key:
..      secretKeyRef:
..      name: hasura-project-secrets
..      key: auth.msg91.key
..      mobileSenderName: "MyAwesomeApp"
..      mobileTemplateRegister: |
..      Welcome to MyAwesomeApp %(name)s! Your OTP is %(otp)s.
..      mobileTemplateVerify: |
..      Hi %(name)s. Verify your phone for your MyAwesomeApp account. Your OTP is %(otp)s.
..      mobileTemplateWelcomeNew: ""
..      mobileTemplateEmailChanged: ""
..      # otp login
..      allowOTPLogin: "" # set empty string if want to set to false; default: false
..      allowOTPSignup: "" # set empty string if want to set to false; default: false
..      # social login
..      # google
..      googleClientId: ""
..      googleClientSecret:
..      secretKeyRef:
..      name: hasura-project-secrets
..      key: auth.google.client_secret
..      googleScopes: ""
..      googleRedirectUri: ""
..      # facebook
..      facebookClientId: ""
..      facebookClientSecret:
..      secretKeyRef:
..      name: hasura-project-secrets
..      key: auth.facebook.client_secret
..      facebookScopes: ""
..      facebookRedirectUri: ""
..      # github
..      githubClientId: ""
..      githubClientSecret:
..      secretKeyRef:
..      name:
..      hasura-project-secrets
..      key:
..      auth.github.client_secret
..      githubScopes:
..      ""
..      githubRedirectUri:
..      ""
..      #
..      linkedin
..      linkedInClientId:
..      ""
..      linkedInClientSecret:
..      secretKeyRef:
..      name:
..      hasura-project-secrets
..      key:
..      auth.linkedin.client_secret
..      linkedInScopes:
..      ""
..      linkedInRedirectUri:
..      ""

.. _gateway-conf:

GatewayConf
^^^^^^^^^^^
.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - httpDirectives
     - false
     - array
     - Array of nginx directive strings. Eg: ``["limit_rate 4k"]``
   * - domains
     - true
     - :ref:`DomainConf <domain-conf>`
     - Object that stores the domains that this project is configured for and whether SSL is enabled or not
   * - httpRoutes
     - true
     - :ref:`HttpRouteConf <http-route-conf>`
     - Object that stores subdomains as keys and their configurations as values


.. _domain-conf:

DomainConf
^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <domain-name> (eg: ``mywebsite.com``)
     - true
     - :ref:`DomainOptions <domain-options>`
     - An object containing the SSL configuration options for this domain

.. _domain-options:

DomainOptions
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - ssl
     - true
     - ``null`` or :ref:`SSLOptions <ssl-options>`
     - The ssl key needs to be present. It's value can be ``null`` or an ``SSLOptions`` object.

.. _ssl-options:

SSLOptions
^^^^^^^^^^
The ``SSLOptions`` object should contain only one key. Either ``LetsEncrypt`` or ``WildCard``.

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - LetsEncrypt
     - false
     - :ref:`LetsEncryptOptions <le-options>`
     - An object with a single key.
   * - Wildcard
     - true
     - :ref:`WildcardOptions <wc-options>`
     - An object that specifies the privatekey and fullchain values

.. _le-options:

LetsEncryptOptions
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - account
     - true
     - string
     - The kubernetes account secret name

.. _wc-options:

WildcardOptions
^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - privkey
     - true
     - string
     - The private key ``pem`` data
   * - fullchain
     - true
     - string
     - The fullchain ``pem`` data

.. _http-route-conf:

HttpRouteConf
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <subdomain> (Eg: ``www`` or ``app`` or ``""`` (empty string) )
     - true
     - :ref:`HTTPRoutes <http-routes>`
     - The key is subdomain and the value is an object denoting all the http-routes for this subdomain

.. _http-routes:

HTTPRoutes
^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <url-path> (Eg: ``/`` or ``/foo/bar``)
     - true
     - :ref:`HTTPRoute <http-route>`
     - The key is subdomain and the value is an object denoting all the http-routes for this subdomain


.. _http-route:

HTTPRoute
^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - upstreamService
     - true
     - string
     - The internal DNS endpoint for the service (Eg: ``postgres.hasura`` or ``app.default``)
   * - upstreamServicePort
     - true
     - string
     - The port at which the service is receiving connections. Will typically be ``80`` for
       new services created via the console
   * - upstreamServicePath
     - true
     - string
     - The path at which the request is made (eg: ``/`` or ``/static``; typically ``/``)
   * - enableCORS
     - true
     - boolean
     - Default ``false`` so CORS is disable for all API requests via the gateway.
   * - restrictToRoles
     - false
     - array
     - Restrict access to the service for users logged in with specific roles only (Eg: ``["admin"]``
       will restrict access to the service for ``admin`` users only; By default, this key is not present)
   * - enableAuth
     - true
     - boolean
     - Convert incoming ``Authorization`` bearer tokens and ``cookies`` into ``X-Hasura-User-Id``,
       ``X-Hasura-Role`` and ``X-Hasura-Allowed-Roles`` headers. By default, set to ``true``
   * - enableWebsockets
     - true
     - boolean
     - Default ``true``
   * - locationDirectives
     - true
     - array
     - Array of nginx location directive strings. (Eg: ``["limit_rate 40k"]``)

.. _ssh-conf:

SSHConf
^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - authorizedKeys
     - true
     - string
     - ``\n`` separated keys (just like the contents of a typical ``.ssh/authorized_keys`` file)

.. _ci-conf:

CIConf
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - remotes
     - true
     - :ref:`RemoteMap <remote-map>`
     - ``\n`` separated keys (just like the contents of a typical ``.ssh/authorized_keys`` file)

.. _remote-map:

RemoteMap
^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <remote-name>
     - true
     - :ref:`DeploymentConfMap <deployment-conf-map>`
     - The key represents the name of the remote and the object represents the configuration options for that remote

.. _deployment-conf-map:

DeploymentConfMap
^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <deployment-name>
     - true
     - :ref:`ImageBuildMap <image-build-map>`
     - The key represents the kubernetes deployment name and the object represents the image build configuration

.. _image-build-map:

ImageBuildMap
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <image-name>
     - true
     - :ref:`DockerBuildOptions <docker-build-options>`
     - The key represents the name of the Docker image created and the value is an object represting docker build configuration
       options

.. _docker-build-options:

DockerBuildOptions
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - path
     - true
     - string
     - The directory that represents 'context' directory for the ``docker build`` command
   * - dockerfile
     - true
     - string
     - The path to the ``Dockerfile`` that is to be built

