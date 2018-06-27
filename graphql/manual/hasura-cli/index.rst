.. .. meta::
   :description: User's manual for using Hasura's command line tooling, hasuractl
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl

.. _hasuracli-manual:

.. highlight:: bash

Hasura CLI
==========

The ``Hasura CLI`` is a command line tool which is the primary mode of managing Hasura projects and migrations.

.. _hasuractl-installation:

Installation
------------

Refer to :doc:`install-hasura-cli`.


Commands
--------

.. toctree::
   :maxdepth: 1
   :titlesonly:

   install-hasura-cli
   Troubleshooting <troubleshooting>
   hasura <hasura>
   hasura completion <hasura_completion>
   hasura console <hasura_console>
   hasura example <hasura_example>
   hasura hub:clone <hasura_hub:clone>
   hasura hub:publish <hasura_hub:publish>
   hasura hub:quickstart <hasura_hub:quickstart>
   hasura hub:search <hasura_hub:search>
   hasura hub:unpublish <hasura_hub:unpublish>
   hasura init <hasura_init>
   hasura login <hasura_login>
   hasura logout <hasura_logout>
   hasura metadata <hasura_metadata>
   hasura metadata apply <hasura_metadata_apply>
   hasura metadata export <hasura_metadata_export>
   hasura metadata reset <hasura_metadata_reset>
   hasura migrate <hasura_migrate>
   hasura migrate apply <hasura_migrate_apply>
   hasura migrate create <hasura_migrate_create>
   hasura migrate status <hasura_migrate_status>
   hasura platform:api-console <hasura_platform:api-console>
   hasura platform:cluster <hasura_platform:cluster>
   hasura platform:cluster add <hasura_platform:cluster_add>   
   hasura platform:cluster create <hasura_platform:cluster_create>   
   hasura platform:cluster delete <hasura_platform:cluster_delete>   
   hasura platform:cluster get-credentials <hasura_platform:cluster_get-credentials>   
   hasura platform:cluster get-default <hasura_platform:cluster_get-default>   
   hasura platform:cluster install <hasura_platform:cluster_install>   
   hasura platform:cluster list <hasura_platform:cluster_list>   
   hasura platform:cluster set-default <hasura_platform:cluster_set-default>   
   hasura platform:cluster status <hasura_platform:cluster_status>   
   hasura platform:cluster template-context <hasura_platform:cluster_template-context>   
   hasura platform:cluster top <hasura_platform:cluster_top>   
   hasura platform:cluster upgrade <hasura_platform:cluster_upgrade>   
   hasura platform:conf <hasura_platform:conf>
   hasura platform:conf apply <hasura_platform:conf_apply>
   hasura platform:conf diff <hasura_platform:conf_diff>
   hasura platform:conf generate-domain <hasura_platform:conf_generate-domain>
   hasura platform:conf generate-remote <hasura_platform:conf_generate-remote>
   hasura platform:conf generate-route <hasura_platform:conf_generate-route>
   hasura platform:help <hasura_platform:help>
   hasura platform:microservice <hasura_platform:microservice>
   hasura platform:microservice apply <hasura_platform:microservice_apply>   
   hasura platform:microservice clone <hasura_platform:microservice_clone>   
   hasura platform:microservice copy <hasura_platform:microservice_copy>   
   hasura platform:microservice create <hasura_platform:microservice_create>   
   hasura platform:microservice exec <hasura_platform:microservice_exec>   
   hasura platform:microservice list <hasura_platform:microservice_list>   
   hasura platform:microservice logs <hasura_platform:microservice_logs>   
   hasura platform:microservice open <hasura_platform:microservice_open>   
   hasura platform:microservice port-forward <hasura_platform:microservice_port-forward>   
   hasura platform:microservice remove <hasura_platform:microservice_remove>   
   hasura platform:microservice restart <hasura_platform:microservice_restart>   
   hasura platform:microservice scale <hasura_platform:microservice_scale>   
   hasura platform:microservice start <hasura_platform:microservice_start>   
   hasura platform:microservice status <hasura_platform:microservice_status>   
   hasura platform:microservice stop <hasura_platform:microservice_stop>   
   hasura platform:microservice sync <hasura_platform:microservice_sync>   
   hasura platform:migration <hasura_platform:migration>
   hasura platform:migration apply <hasura_platform:migration_apply>   
   hasura platform:migration create <hasura_platform:migration_create>   
   hasura platform:migration db-reset <hasura_platform:migration_db-reset>   
   hasura platform:migration status <hasura_platform:migration_status>   
   hasura platform:secret <hasura_platform:secret>
   hasura platform:secret list <hasura_platform:secret_list>   
   hasura platform:secret update <hasura_platform:secret_update>   
   hasura platform:setup <hasura_platform:setup>
   hasura platform:ssh-key <hasura_platform:ssh-key>
   hasura platform:ssh-key add <hasura_platform:ssh-key_add>   
   hasura platform:ssh-key list <hasura_platform:ssh-key_list>   
   hasura platform:ssh-key remove <hasura_platform:ssh-key_remove>   
   hasura update-cli <hasura_update-cli>
   hasura user-info <hasura_user-info>
   hasura version <hasura_version>
