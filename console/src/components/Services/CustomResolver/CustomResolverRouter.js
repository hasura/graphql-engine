import React from 'react';

import { Route, IndexRedirect } from 'react-router';

import { LayoutWrapper, rightBar } from '../Layout';

import { CustomResolver, Add } from '.';

// Objective is to render list of custom resolvers on the
// left nav bar.
// Custom resolvers list is fetched from hdb_catalog/custom_resolver
// Whenever any operation happens like add resolver/delete resolver, this state should update automatically.

const getCustomResolverRouter = connect => {
  return (
    <Route path="custom-resolver" component={LayoutWrapper}>
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightBar(connect)}>
        <IndexRedirect to="resolvers" />
        <Route path="resolvers" component={CustomResolver} />
        <Route path="add" component={Add} />
      </Route>
    </Route>
  );
};

export default getCustomResolverRouter;
