import * as React from 'react';
import { connect } from 'react-redux';
import Helmet from 'react-helmet';
import { RouteComponentProps } from 'react-router';
import generatedApiExplorer from './ApiExplorer';

import TopNav from './TopNav';

type ContainerProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
};

const ApiExplorer: React.FC = generatedApiExplorer(connect);

const Container: React.FC<ContainerProps> = props => {
  const { location, children } = props;
  return (
    <div className='flex flex-col flex-1'>
      <Helmet title="API Explorer | Hasura" />
      <TopNav location={location} key='top-bar' />
      {children || <ApiExplorer {...props} key='api-explorer' />}
    </div>
  );
};

export default Container;
