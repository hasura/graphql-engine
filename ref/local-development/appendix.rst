Appendix
========

Installing kubectl
------------------

See: http://kubernetes.io/docs/user-guide/prereqs/


.. _k8s_cluster:

Setting up Kubernetes cluster locally
-------------------------------------

1. Use minikube to setup your cluster. Read details here:
   https://github.com/kubernetes/minikube
2. Use Vagrant and CoreOS to setup a Kubernetes cluster. Read about it here:
   https://coreos.com/kubernetes/docs/latest/kubernetes-on-vagrant-single.html


.. _config_dns:

Configuring local DNS
---------------------

As the Hasura project is available at ``*.hasura.test`` domain, we need to
configure a local DNS to resolve that address. For this we will use `dnsmasq`_.

Follow the steps to install and configure ``dnsmasq``:

* Download and install `dnsmasq`_ by following respective instructions for your
  OS.

* Once `dnsmasq`_ is installed. There are two main ways of configuring your
  system to use the installed dnsmasq as one of your DNS providers.

  Use only one of the following:

  * Add ``dnsmasq`` as resolver in ``/etc/resolv.conf`` manually:

    .. code-block:: cfg

       nameserver 127.0.0.1
       nameserver ...

  * If you're on Linux and running `NetworkManager`_, you can configure your
    `NetworkManager`_ to use ``dnsmasq``. Edit the
    ``/etc/NetworkManager/NetworkManger.conf`` file and change the
    ``dns=default`` line to:

    .. code-block:: cfg

       dns=dnsmasq

    and restart ``NetworkManger`` service.

    .. code-block:: shell

       $ sudo systemctl restart NetworkManager.service

* Get the IP address of your Kubernetes cluster. Then edit
  ``/etc/dnsmasq.conf`` and add the following line at the end of the file.

  .. code-block:: cfg

     address=/.hasura.test/192.168.99.101

  where ``192.168.99.101`` is the IP address of your Kubernetes cluster.

* Restart the ``dnsmasq`` service to take effect.

  .. code-block:: shell

     $ sudo systemctl restart dnsmasq.service

* Try the following command to see if it resolves properly:

  .. code-block:: shell

      $ nslookup hasura.test

  You should get an output similar to:

  .. code-block:: shell

      Server:         127.0.0.1
      Address:        127.0.0.1#53

      Name:   hasura.test
      Address: 192.168.99.101


----

.. admonition:: Check out!

    There are some `scripts here <https://github.com/hasura/dnsmasq-hasura>`_
    for different OS platforms to install and configure `dnsmasq`_.


Admin credentials
-----------------

Admin credentials for the local project is specified below.

.. list-table::

  * - Username of project console
    - admin
  * - Password of project console
    - adminpassword
  * - Postgres User
    - admin
  * - Postgres Password
    - postgresadmin

----

.. _dnsmasq: http://www.thekelleys.org.uk/dnsmasq/doc.html
.. _NetworkManager: https://wiki.gnome.org/Projects/NetworkManager
