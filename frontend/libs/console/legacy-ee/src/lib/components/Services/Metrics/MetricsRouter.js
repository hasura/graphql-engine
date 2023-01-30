import React from 'react';

import { Route, IndexRoute } from 'react-router';

import {
  metricsConnector,
  errorConnector,
  operationsConnector,
  usageConnector,
  allowListsConnector,
  regressionTestsConnector,
  websocketsConnector,
  subscriptionWorkersConnector,
  ApiLimits,
} from '.';
import overviewConnector from './Overview';

import { moduleName } from './constants';

const metricsRouter = connect => {
  return (
    <Route path={moduleName} component={metricsConnector(connect)}>
      <IndexRoute component={overviewConnector(connect)} />
      <Route path="error" component={errorConnector(connect)} />
      <Route path="operations" component={operationsConnector(connect)} />
      <Route path="usage" component={usageConnector(connect)} />
      <Route path="allow-lists" component={allowListsConnector(connect)} />
      <Route
        path="regression-tests"
        component={regressionTestsConnector(connect)}
      />
      <Route path="api-limits" component={ApiLimits} />
      <Route path="websockets" component={websocketsConnector(connect)} />
      <Route
        path="subscription-workers"
        component={subscriptionWorkersConnector(connect)}
      />
    </Route>
  );
};

export default metricsRouter;
