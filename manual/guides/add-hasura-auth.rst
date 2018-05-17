.. meta::
   :description: A guide to declaratively hasura auth to your react application using react-check-auth
   :keywords: hasura, guide, auth, authentication, react-check-auth, react authentication,
   :content-tags: app architecture, build apps

Add auth checks to your react/react-native applications
==========================================================

Introduction
------------

Lets say we have a simple todo application with three simple routes as follows

.. code-block:: js

  <Router>
  	<Route path="/register" component={Register}/>
  	<Route path="/login" component={Login}/>
  	<ProtectedRoute path="/" component={TodoApp}/>
  </Router>

We have two unprotected routes ``/login`` and ``/register`` and one protected route at ``/``. The routes ``/login`` and ``/register`` will be accessible by all the users and the route ``/`` should be accessible only by authenticated users. Now how do we make sure that route is only accessible by authenticated users?

There are multiple ways to solve the problem. In this guide we will take a look at how you can add authentication using Hasura's `react-check-auth <https://github.com/hasura/react-check-auth>`_

.. note::

  react-check-auth uses React's context API which is part of the react 16.3 release. It shouldn't be used with applications using React version < 16.3

Step 1: Installation
--------------------

.. code-block:: bash

   $ npm install react-check-auth --save

Step 2: Usage
-------------------------------------------

Step 2.1: Add AuthProvider
^^^^^^^^^^^^^^^^^^^^^^^^^^

Wrap your ``ProtectedRoute`` component with ``AuthProvider`` from ``react-check-auth`` that has an endpoint to fetch basic user information. This works because if the user had logged in, a cookie would already be present. For using authorization headers, read more on `react-check-auth <https://github.com/hasura/react-check-auth>`_.

.. code-block:: js

  import React from "react";
  import ReactDOM from "react-dom";

  import {AuthProvider} from "react-check-auth";
  
  const ProtectedRoute = () => (
    <AuthProvider authUrl={'https://auth.<cluster_name>.hasura-app.io/v1/user/info'}>
      // Rest of the code goes here
    </AuthProvider>
  );
  
Step 2.2: Access user information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now, in your ``ProtectedRoute`` component, you can route the user accordingly depending on whether the user is currently logged in or not. Here we are using ``AuthConsumer`` from ``react-check-auth``. AuthConsumer accepts a renderProp which it will call with three parameters ( userInfo, isLoading, error ).

.. code-block:: js

  import {AuthConsumer} from 'react-check-auth';
  import {AuthProvider} from "react-check-auth";
  import { Route, Redirect } from 'react-router';
  import TodoApp from './todoapp';
  
  const ProtectedRoute = () => (
    <AuthProvider authUrl={'https://auth.<cluster_name>.hasura-app.io/v1/user/info'}>
      // Rest of the code goes here
      <AuthConsumer> 
        {({userInfo, isLoading, error}) => ( 
          userInfo ?
            (<Route path="/" component={ TodoApp }></Route>) :
            (
              <Redirect to='/login' />
            )
        )}
       </AuthConsumer>
    </AuthProvider>
  );

If you haven't implemented login/signup pages, you can use the :doc:`Auth UI Kit <../auth/auth-ui-kit/index>`. Take a look at the usage of Auth UI Kit url in the code sample below

.. code-block:: js

  import {AuthConsumer} from 'react-check-auth';
  import {AuthProvider} from "react-check-auth";
  import { Route, Redirect } from 'react-router';
  import TodoApp from './todoapp';
  
  const ProtectedRoute = () => (
    <AuthProvider authUrl={'https://auth.<cluster_name>.hasura-app.io/v1/user/info'}>
      // Rest of the code goes here
      <AuthConsumer> 
        {({userInfo, isLoading, error}) => ( 
          userInfo ?
            (<Route path="/" component={ TodoApp }></Route>) :
            (
              <a href="https://auth.<cluster_name>.hasura-app.io/ui">Login</a>
            )
        )}
       </AuthConsumer>
    </AuthProvider>
  );
