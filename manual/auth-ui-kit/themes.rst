
Auth UI Kit Themes
==================

The Auth UI kit comes with two theme options to choose from. The dark theme is enabled by default.

.. figure:: ../../img/uikit-dark.png
   :class: 'dark'
.. figure:: ../../img/uikit-light.png
   :class: 'light'


Changing the theme:
-------------------

The UI kit theme can be changed by editing the :doc:`conf/auth.yaml <../project/directory-structure/conf/auth.yaml>` file in your hasura project.

.. code-block:: yaml
   :emphasize-lines: 2

   uiKit:
     theme: "dark"
     ...

Options for theme are ``light`` and ``dark`` with ``dark`` being the default one.
