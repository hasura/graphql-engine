import React, { useState, useEffect } from 'react';
import semverCheck from '../../../helpers/semver';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

const useMetadataSemver = serverVersion => {
  const [
    supportInconsistentMetadata,
    setSupportInconsistentMetadata,
  ] = useState(false);
  useEffect(() => {
    if (serverVersion) {
      setSupportInconsistentMetadata(
        semverCheck('inconsistentState', serverVersion)
      );
    }
  }, [serverVersion]);
  return {
    supportInconsistentMetadata,
  };
};

const Container = ({ location, serverVersion, children, metadata }) => {
  const { supportInconsistentMetadata } = useMetadataSemver(
    serverVersion
  );
  const sidebar = (
    <Sidebar
      supportInconsistentMetadata={supportInconsistentMetadata}
      location={location}
      metadata={metadata}
    />
  );
  const helmet = 'Metadata | Hasura';
  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, {
      supportInconsistentMetadata,
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
