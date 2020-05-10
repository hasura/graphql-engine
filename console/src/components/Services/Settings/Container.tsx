import * as React from 'react';
import { Connect } from 'react-redux';
import { LocationShape } from 'react-router/lib/PropTypes';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

type Metadata = {
  inconsistentObjects: never[];
  ongoingRequest: boolean;
  allowedQueries: never[];
};

type ContainerProps = {
  location: LocationShape;
  metadata: Metadata;
  children: JSX.Element;
};

const Container: React.FC<ContainerProps> = ({
  location,
  children,
  metadata,
}) => {
  const helmet = 'Settings | Hasura';

  const sidebar = <Sidebar location={location} metadata={metadata} />;

  const childrenWithProps = React.Children.map(
    children,
    (child: React.ReactElement<any>) => React.cloneElement(child, { metadata })
  );
  return (
    <PageContainer helmet={helmet} leftContainer={sidebar}>
      {childrenWithProps}
    </PageContainer>
  );
};

const mapStateToProps = (state: any) => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const connector = (connect: Connect) => connect(mapStateToProps)(Container);

export default connector;
