.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

=========================
Installing the hasura CLI
=========================

Step 1: Install
---------------

.. tabs::

   tabs:
     - id: linux
       content: |

         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         **Note:** You should be running 64-bit windows, and should have ``git`` installed to run the ``hasura`` CLI.

         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://hasura.io/install.msi>`_


Step 2: Login
-------------

.. tabs::

   tabs:
     - id: linux
       content: |

         Next, login or register by running the following command:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

     - id: mac
       content: |
         Next, login or register by running the following command:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

     - id: windows
       content: |
         Next, open the command prompt, or ``git-bash`` and login/register:

         .. code-block:: bash

            hasura.exe login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

Adding shell completion (optional)
----------------------------------
You can also add ``bash`` or ``zsh`` completions for ``hasura`` CLI.

Bash
~~~~

Linux
+++++
Generate the Bash completion using:

.. code-block:: bash

  $ sudo hasura completion bash --file=/etc/bash.completion.d/hasura

Mac
+++

1. Install bash-completion using homebrew:

.. code-block:: bash

  $ brew install bash-completion

2. Add the following code to your ``~/.bash_profile``:

.. code-block:: bash

  if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
  fi

3. Add hasura completion:

.. code-block:: bash

  $ sudo hasura completion bash --file=/etc/bash_completion.d/hasura


Zsh
~~~
Execute the following commands:

.. code-block:: bash

  $ mkdir -p $HOME/.oh-my-zsh/completions
  $ hasura completion zsh --file=$HOME/.oh-my-zsh/completions/_hasura


..     
  - id: windows
       content: |
         **Note:** You should be running 64-bit windows, and should have ``git`` installed to run the ``hasura`` CLI.
         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://hasura.io/install.msi>`_
     - id: windows
       content: |
         Next, open the command prompt, or ``git-bash`` and login/register:
         .. code-block:: bash
            hasura.exe login
         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

