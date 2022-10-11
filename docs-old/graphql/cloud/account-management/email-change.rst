.. meta::
   :description: Hasura Cloud Email Change
   :keywords: hasura, docs, cloud, email

Change email address on Hasura Cloud
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can edit the email address on your Hasura Cloud account to another email from the ``Account Settings`` page.

.. _email_change:

Changing email for users logged in using email
----------------------------------------------

`Sign in <https://cloud.hasura.io/login?redirect_url=/>`__ to your Hasura Cloud account using email and click ``My Account``.
On the ``Account Settings`` page, select the ``Edit`` button.

.. thumbnail:: /img/graphql/cloud/account-settings/account-settings-tab.png
  :alt: Account Management
  :width: 1100px

Enter the email you want to transfer the account to and click the ``Change`` button to send a transfer request.

.. thumbnail:: /img/graphql/cloud/account-settings/edit-email-input.png
   :alt: edit email section
   :width: 800px

The invitee receives an email verification mail. Once the invitee clicks the link, it logs out the existing user and redirects the invitee to the Hasura Cloud login page.

The user can now log in to Hasura Cloud using the new email and the password for the old email.

Changing email for users with social logins
-------------------------------------------

If you logged in to Hasura Cloud with your social login, you must reset your password to initiate the email change process.
On the `Sign in <https://cloud.hasura.io/login?redirect_url=/>`__ page, select ``Forgot?``.

.. thumbnail:: /img/graphql/cloud/account-settings/forgot-password.png
   :alt: Forgot password
   :width: 450px

Next, enter the new email id and click ``Recover Password``.
Set a new password by clicking on the reset password link sent to your email address.

You can now log in with the email and this new password and follow the steps mentioned in the :ref:`above section <email_change>` to change the email.

You can also use the social login associated with the new email to log in to Hasura Cloud!
