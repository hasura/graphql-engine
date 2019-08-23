import React from 'react';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

const Container = ({ location, children, metadata }) => {
  const helmet = 'Metadata | Hasura';

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

const connector = connect => connect(mapStateToProps)(Container);

export default connector;
