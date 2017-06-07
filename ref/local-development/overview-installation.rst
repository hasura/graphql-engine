Overview and Installation
=========================

Overview
--------

We offer a command-line tool (``hasuractl``) to install a Hasura project
locally on your computer. You can use it to try out Hasura and also use it for
local development of your project.

``hasuractl`` assumes that you have a Kubernetes cluster running on your
machine; it will then install a Hasura project on that cluster.

This can be done in many ways. You can use `minikube`_, or use `Vagrant`_ to
setup a Kubernetes cluster. See :ref:`here <k8s_cluster>` for more on creating
Kubernetes cluster.

This guide assumes you have `minikube`_ installed and using it for creating the
Kubernetes cluster.

You can then use ``hasuractl`` to install and manage projects on that
Kubernetes cluster.


Installation
------------

Pre-requistites
^^^^^^^^^^^^^^^

The following software is required to run a Hasura project. Make sure you
install them before proceeding.

* Python >= 3.2
* `pip`_ : to install ``hasuractl``
* `dnsmasq`_ : to run a local DNS server

And optionally:

* `minikube`_ : to create Kubernetes cluster
* `kubectl`_ : to interact with the Kubernetes cluster


Installation
^^^^^^^^^^^^

* Ensure you have installed `pip`_. Then run:
.. code-block:: shell

   $ pip install hasuractl

This will install ``hasuractl`` into your path.  This might require *super user
privileges*.

You can also use the ``--user`` option of ``pip`` to install ``hasuractl`` in
your home directory and not globally. Then you might have to add the path to
your ``PATH`` environment variable.

You can optionally also use `virtualenv`_.

* Type ``hasuractl --version`` to check if everything is working properly.



.. _minikube: https://github.com/kubernetes/minikube
.. _Vagrant: https://coreos.com/kubernetes/docs/latest/kubernetes-on-vagrant-single.html
.. _kubectl: http://kubernetes.io/docs/user-guide/kubectl-overview/
.. _pip: https://pip.pypa.io/en/stable/installing/
.. _virtualenv: https://virtualenv.pypa.io/en/stable/
.. _dnsmasq: http://www.thekelleys.org.uk/dnsmasq/doc.html
