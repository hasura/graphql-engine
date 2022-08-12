import * as React from 'react';
import { Connect } from 'react-redux';
import { RouteComponentProps } from 'react-router';
import Sidebar from './Sidebar';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';

type Metadata = {
  inconsistentObjects: Record<string, unknown>[];
  inconsistentInheritedRoles: Record<string, unknown>[];
  ongoingRequest: boolean;
  allowedQueries: Record<string, any>[];
};

type ExternalProps = {
  location: RouteComponentProps<unknown, unknown>['location'];
  children: JSX.Element;
};

interface ContainerProps extends StateProps, ExternalProps {}

const Container: React.FC<ContainerProps> = ({
  location,
  children,
  metadata,
}) => {
  const helmet = 'Settings | Hasura';

  const sidebar = <Sidebar location={location} metadata={metadata} />;

  const childrenWithProps = React.Children.map(
    children,
    (child: React.ReactElement<{ metadata: Metadata }>) =>
      React.cloneElement(child, { metadata })
  );
  return (
    <PageContainer helmet={helmet} leftContainer={sidebar}>
      {childrenWithProps}
    </PageContainer>
  );
};

type DerivedState = {
  main: Record<string, any>;
  metadata: Metadata;
  tables: Record<'dataHeaders', Record<string, string>>;
};

const mapStateToProps = (state: DerivedState) => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

type StateProps = ReturnType<typeof mapStateToProps>;

const connector = (connect: Connect) =>
  connect<StateProps, unknown, unknown, DerivedState>(mapStateToProps)(
    Container
  );

export default connector;
