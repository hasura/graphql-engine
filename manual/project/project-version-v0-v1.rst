.. _hasura-project-version

Hasura project version: v0 & v1
===============================

Hasura CLI versions beyond and including ``v0.2.54`` will convert any project into ``v1`` version. All existing projects are considered to be version ``v0``.

What changed in v1?
-------------------

- Hasura project version ``v1`` is identified by the presence of ``version: v1`` in ``hasura.yaml``.
- Pro-tier clusters can only be added to a ``v1`` project.
- Info regarding clusters are split across ``clusters.yaml`` and ``.hasura``
- Cluster infrastructure configuration and an identifying alias is present in ``clusters.yaml``.
- Details about clusters added to the project, including name are present in ``.hasura``.
- Cluster alias (``alias``) is the key that links clusters in both files.
