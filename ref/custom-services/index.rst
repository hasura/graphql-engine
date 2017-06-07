.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Deploy code with git push
=========================

To deploy your code as a service using git-push you need to do the following:

#. Create a git repo with a ``Dockerfile`` in it
#. Add your SSH key to your VM via the Hasura console so that your computer is authorized to push code
#. Create a service via the Hasura console with git-push enabled

Creating a git repo
-------------------

The best way to get a base setup ready, is to grab the relevant 
base template directory from `git-push-templates <https://github.com/hasura/git-push-templates>`_

This is what your directory structure should look like::

   myrepo/
      Dockerfile
      .git
      ...

Note the ``Dockerfile`` at the top level. This Dockerfile is used by the Hasura platform
automatically to build your code in the right environment.

Adding your SSH key
-------------------

Once you've generated an SSH key, you need to copy your public key and paste it in the authorized_keys section
in the Advanced page on the Hasura console.

.. admonition:: Generating SSH keys

   In case you don't have an SSH key or aren't sure if you have one already, follow this guide:
   `Creating SSH Keys <https://confluence.atlassian.com/bitbucketserver/creating-ssh-keys-776639788.html>`_

Once you have your SSH key, copy it to your clipbard. You can try any of the commands below to copy
it to your clipboard:

.. code-block:: console

   $ cat ~/.ssh/id_rsa.pub | pbcopy       # Mac

   $ clip < ~/.ssh/id_rsa.pub             # Windows

   $ sudo apt-get install xclip           # Linux
   $ xclip -sel clip < ~/.ssh/id_rsa.pub  # Linux

Then, head to ``console.<project-name>.hasura-app.io/advanced#sshkeys``, copy it to authorized_keys and save it!

.. image:: sshkeys.png
   :scale: 50%


Adding a git-push enabled service
---------------------------------

In the Add custom service section of the Hasura console, ensure that git-push is enabled and you're good to go.

For reference, here's a configuraiton screenshot:

.. image:: ../../getting-started/gitpush.png
   :scale: 50%
