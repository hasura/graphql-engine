import React, { useState, useEffect } from 'react';
import semverCheck from '../../../helpers/semver';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

const Container = ({ location, serverVersion, children, metadata }) => {
  const sidebar = (
    <Sidebar
      location={location}
      metadata={metadata}
    />
  );
  const helmet = 'Metadata | Hasura';
  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, {
      metadata,
    })
  );
  return (
    <PageContainer helmet={helmet} leftContainer={sidebar}>
      {childrenWithProps}
    </PageContainer>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
    serverVersion: state.main.serverVersion,
  };
};

const connector = connect => connect(mapStateToProps)(Container);

export default connector;
