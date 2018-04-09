Moving to the new hasura CLI
============================
From Hasura v0.15, the workflow for Hasura has changed considerably.

If you were using ``hasuractl`` before, you have to update to the new
``hasura`` CLI tool.

For this, you have to delete your old ``hasuractl``, and then install the new
``hasura`` CLI tool.

Step 1: Remove old hasuractl
----------------------------

Linux & Mac
~~~~~~~~~~~
Run the following command:

.. code:: shell

  $ rm $(which hasuractl)

.. note::
  ``sudo`` might be required depending on how you installed it.

Windows
~~~~~~~
1. Open ``cmd``, type ``which hasuractl``.
2. Go to the path, where it is installed.
3. Delete the file.


Step 2: Install new hasura CLI
------------------------------
Install the new ``hasura`` CLI by referring to :doc:`../install-hasura-cli`.


Step 3: Get familiar with the new workflow
------------------------------------------
:doc:`Get started <../getting-started/index>` with the new ``hasura``.

Read about :doc:`moving from old hasuractl workflow to the new one <workflow>`.
