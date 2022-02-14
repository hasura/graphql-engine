.. meta::
   :description: Manage Payment Methods
   :keywords: hasura, cloud, docs, payment, cards, manage cards, payment methods, manage payment methods

.. _payment_methods:

Manage payment methods
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can add one or more cards as a payment method for upcoming billing cycles on your Hasura Cloud account.

Add a new card
--------------

Go to the billing section, and click on ``View Cards``. 

.. thumbnail:: /img/graphql/cloud/billing/manage_cards.png
   :alt: view saved cards

Click the ``+`` sign to add a new card. 

.. thumbnail:: /img/graphql/cloud/billing/add_new_card.png
   :alt: add a new card
   :width: 437px

After adding appropriate values, click ``Save``.

Setting a default card
----------------------

The default card is used for the payment of upcoming billing cycles.

Select the card you want to set as the default payment method or add a new card, check the ``Set Card as Default`` option and click ``Save``. 

.. thumbnail:: /img/graphql/cloud/billing/set_existing_default.png
   :alt: set an exiting card as the default card
   :width: 437px

.. thumbnail:: /img/graphql/cloud/billing/set_new_default.png
   :alt: Add a new card as the default card
   :width: 437px

Delete a card
-------------

Select the card you want to delete and click the ``Remove Card`` option. 

.. thumbnail:: /img/graphql/cloud/billing/delete_card.png
   :alt: Delete card
   :width: 437px

.. note::

  A card set as default can not be deleted if the user has billable projects or pending dues. 
  
   
