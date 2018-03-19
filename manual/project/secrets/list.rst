Listing project secrets
=======================

To see all the secrets that are saved on your cluster:

.. code-block:: bash

   $ hasura secrets list  # optionally -c <cluster-alias>

   INFO Fetching secrets...
   auth.secretKey----------------|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   notify.sparkpost.key----------| 
   notify.twilio.accountsid------| 
   auth.linkedin.client_secret---| 
   auth.github.client_secret-----| 
   auth.sparkpost.key------------| 
   postgres.password-------------|xxxxxxxxxxx-xxxxxxxxxxx-xxx-xxxxxxxxxx
   postgres.user-----------------|admin
   auth.msg91.key----------------| 
   notify.twilio.authtoken-------| 
   notify.msg91.key--------------| 
   auth.facebook.client_secret---| 
   auth.google.client_secret-----| 
   ssh.authorizedKeys------------| 
   notify.smtp.password----------| 
   notify.smtp.username----------| 
   auth.recaptcha.secret---------| 
   auth.admin.password-----------|xxxxxxxxxxxx-xxxxxxxxxxxxx-xxxxxxxxx-xxxx