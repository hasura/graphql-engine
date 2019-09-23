Security and Disclosure
=======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This page describes Hasura security and disclosure information.

Security Announcements
----------------------

Join the `Hasura Security Announcements <https://groups.google.com/forum/#!forum/hasura-security-announce>`_ group for emails about security and major API announcements.

Reporting vulnerabilities
-------------------------

We’re extremely grateful for security researchers and users that report vulnerabilities to the Hasura community. All reports are thoroughly investigated by a set of community volunteers and the Hasura team.

To report a security issue, please email us at build@hasura.io with all the details, attaching all necessary information.

When should I report a vulnerability?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- You think you have discovered a potential security vulnerability in the Hasura GraphQL engine or related components.
- You are unsure how a vulnerability affects the Hasura GraphQL engine.
- You think you discovered a vulnerability in another project that Hasura GraphQL engine depends on (e.g. Heroku, Docker, etc).
- You want to report any other security risk that could potentially harm Hasrua GraphQL engine users.

When should I NOT report a vulnerability?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- You need help tuning Hasura GraphQL engine components for security.
- You need help applying security related updates.
- Your issue is not security related.

Security vulnerability response
-------------------------------

Each report is acknowledged and analyzed by the project’s maintainers and the security team within 3 working days.

The reporter will be kept updated at every stage of the issue’s analysis and resolution (triage -> fix -> release).

Public disclosure timing
------------------------

A public disclosure date is negotiated by the Hasura product security team and the bug submitter. We prefer to fully disclose the bug as soon as possible once a user mitigation is available. 
It is reasonable to delay disclosure when the bug or the fix is not yet fully understood, the solution is not well-tested, or for vendor coordination. 
The timeframe for disclosure is from immediate (especially if it’s already publicly known) to a few weeks. 
We expect the time-frame between a report to a public disclosure to typically be in the order of 7 days. 
The Hasura GraphQL engine maintainers and the security team will take the final call on setting a disclosure date.

(Some sections have been inspired and adapted from https://github.com/kubernetes/website/blob/master/content/en/docs/reference/issues-security/security.md.)