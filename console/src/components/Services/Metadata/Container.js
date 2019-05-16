import React, { useState, useEffect } from 'react';
import semverCheck from '../../../helpers/semver';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

const useMetadataSemver = serverVersion => {
  const [supportMetadata, setSupportMetadata] = useState(false);
  const [
    supportInconsistentMetadata,
    setSupportInconsistentMetadata,
  ] = useState(false);
  const [supportAllowedQueries, setSupportAllowedQueries] = useState(false);

  useEffect(() => {
    if (serverVersion) {
      setSupportMetadata(semverCheck('metadataReload', serverVersion));
    }
  }, [serverVersion]);

  useEffect(() => {
    if (serverVersion) {
      setSupportInconsistentMetadata(
        semverCheck('inconsistentState', serverVersion)
      );
    }
  }, [serverVersion]);

  useEffect(() => {
    if (serverVersion) {
      setSupportAllowedQueries(semverCheck('allowedQueries', serverVersion));
    }
  }, [serverVersion]);

  return {
    supportMetadata,
    supportInconsistentMetadata,
    supportAllowedQueries,
  };
};

const Container = ({ location, serverVersion, children, metadata }) => {
  const {
    supportMetadata,
    supportInconsistentMetadata,
    supportAllowedQueries,
  } = useMetadataSemver(serverVersion);

  if (!supportMetadata) {
    return null;
  }

  const sidebar = (
    <Sidebar
      semverChecks={{
        supportMetadata,
        supportInconsistentMetadata,
        supportAllowedQueries,
      }}
      location={location}
      metadata={metadata}
    />
  );

  const helmet = 'Metadata | Hasura';

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, {
      supportMetadata,
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
