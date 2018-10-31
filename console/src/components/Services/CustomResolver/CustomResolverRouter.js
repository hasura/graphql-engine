import React from 'react';

import { Route, IndexRedirect } from 'react-router';

import { LayoutWrapper, rightBar } from '../Layout';

import { CustomResolver, Add } from '.';

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
