.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. meta::
   :description: Homepage of the reference documentation for Hasura's Auth service and Table of content
   :keywords: hasura, docs, auth, home, toc, table of contents

Auth
====

Introduction
------------

Auth is one of the Hasura's microservices that is packaged with the platform. It handles authentication and user management use-cases for applications without developers having to write a single line of code. It offers the following features:

+ Register using username, email or mobile number.
+ Login using username, email or mobile number.
+ Email verification.
+ Verification of mobile number via OTP.
+ Forgot password/reset password functionality.
+ Updation of account information.
+ Third-party logins - Google, Facebook, LinkedIn.
+ Role based APIs.

.. toctree::
  :maxdepth: 2

  how-it-works/index
  configuration
  api
  faq
