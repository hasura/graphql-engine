import React from 'react';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import { connect } from 'react-redux';

const Container = ({ location, children, metadata }) => {
  const helmet = 'Settings | Hasura';

  const sidebar = <Sidebar location={location} metadata={metadata} />;

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, { metadata })
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
  };
};

const ConnectedSettings = connect(mapStateToProps)(Container);
export default ConnectedSettings;
