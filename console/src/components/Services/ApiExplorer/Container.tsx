import * as React from 'react';
import Helmet from 'react-helmet';
import { RouteComponentProps } from 'react-router';

import TopBar from './TopNav';

type ContainerProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const Container: React.FC<ContainerProps> = ({ location, children }) => (
  <>
    <Helmet title="API Explorer | Hasura" />
    <div id="left-bar">
      <TopBar location={location} />
    </div>
    <div id="right-bar">{children}</div>
  </>
);

export default Container;
