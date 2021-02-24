import * as React from 'react';
import { connect } from 'react-redux';
import Helmet from 'react-helmet';
import { RouteComponentProps } from 'react-router';
import generatedApiExplorer from './ApiExplorer';

import TopBar from './TopNav';

type ContainerProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const ApiExplorer: React.FC = generatedApiExplorer(connect);

const Container: React.FC<ContainerProps> = props => {
  const { location, children } = props;
  return (
    <>
      <Helmet title="API Explorer | Hasura" />
      <div id="left-bar">
        <TopBar location={location} />
      </div>
      <div id="right-bar">{children || <ApiExplorer {...props} />}</div>
    </>
  );
};

export default Container;
