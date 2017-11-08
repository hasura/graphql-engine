Project Info
============

* Project Page: https://github.com/pypa/pip
* Install howto: http://www.pip-installer.org/en/latest/installing.html
* Changelog: http://www.pip-installer.org/en/latest/news.html
* Bug Tracking: https://github.com/pypa/pip/issues
* Mailing list: http://groups.google.com/group/python-virtualenv
* Docs: http://www.pip-installer.org/
* User IRC: #pip on Freenode.
* Dev IRC: #pypa on Freenode.

Quickstart
==========

First, :doc:`Install pip <installing>`.

Install a package from `PyPI`_:

::

  $ pip install SomePackage
    [...]
    Successfully installed SomePackage

Show what files were installed:

::

  $ pip show --files SomePackage
    Name: SomePackage
    Version: 1.0
    Location: /my/env/lib/pythonx.x/site-packages
    Files:
     ../somepackage/__init__.py
     [...]

List what packages are outdated:

::

  $ pip list --outdated
    SomePackage (Current: 1.0 Latest: 2.0)

Upgrade a package:

::

  $ pip install --upgrade SomePackage
    [...]
    Found existing installation: SomePackage 1.0
    Uninstalling SomePackage:
      Successfully uninstalled SomePackage
    Running setup.py install for SomePackage
    Successfully installed SomePackage

Uninstall a package:

::

  $ pip uninstall SomePackage
    Uninstalling SomePackage:
      /my/env/lib/pythonx.x/site-packages/somepackage
    Proceed (y/n)? y
    Successfully uninstalled SomePackage


.. _PyPI: http://pypi.python.org/pypi/


