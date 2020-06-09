.. meta::
   :description: Hasura security vulnerability protocol
   :keywords: hasura, docs, security, security disclosure, vulnerability

.. _security_protocol:

Security vulnerability protocol
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. inspired and adapted from https://kubernetes.io/docs/reference/issues-security/security/ (https://github.com/kubernetes/website/blob/master/content/en/docs/reference/issues-security/security.md)

Introduction
------------

This page describes the Hasura security vulnerability reporting and disclosure process.

Security announcements
----------------------

Join the `Hasura Security Announcements <https://groups.google.com/forum/#!forum/hasura-security-announce>`_ group for emails about security announcements.

Reporting vulnerabilities
-------------------------

We’re extremely grateful for security researchers and users who report vulnerabilities to the Hasura community. All reports are thoroughly investigated by the Hasura team.

To report a security issue, please email us at build@hasura.io with details, if possible attaching relevant information. The more details we have, the quicker will we be able to fix potential vulnerabilities.

When should I report a vulnerability?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- You think you have discovered a potential security vulnerability in the Hasura GraphQL engine or related components.
- You are unsure how a vulnerability affects the Hasura GraphQL engine.
- You think you discovered a vulnerability in another project that Hasura GraphQL engine depends on (e.g. Heroku, Docker, etc).
- You want to report any other security risk that could potentially harm Hasura GraphQL engine users.

When should I NOT report a vulnerability?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- You need help tuning Hasura GraphQL engine components for security.
- You need help applying security related updates.
- Your issue is not security related.

In these cases you can join our `Discord server <http://hasura.io/discord>`_ where the community will be happy to help you out.

Vulnerability report response
-----------------------------

Each vulnerability report is acknowledged and analyzed by the Hasura team within 3 working days.

The reporter will be kept updated at every stage of the issue’s analysis and resolution (triage -> fix -> release).

Vulnerability public disclosure timing
--------------------------------------

A public disclosure date in case a vulnerability is discovered is negotiated by the Hasura team and the bug submitter.

We prefer to fully disclose the vulnerability as soon as possible once a user mitigation is available and enough of the affected instances have been upgraded.

It is reasonable to delay disclosure when the vulnerability or the fix is not yet fully understood, the solution is not well-tested, or for vendor coordination.
The time frame for disclosure is from immediate (especially if the vulnerability is already publicly known) to a few weeks.
Though, we expect the time frame between a report to a public disclosure to typically be in the order of 7 days.

In any case, the Hasura team will do their best to identify and fix any vulnerabilities as soon as possible, as well as communicate to the submitter about the progress and set a disclosure date. 
