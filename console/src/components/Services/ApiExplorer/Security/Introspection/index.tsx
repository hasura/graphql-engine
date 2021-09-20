import React, { useEffect } from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { SecurityTabs } from '../SecurityTabs';
import { Dispatch, ReduxState } from '../../../../../types';
import { rolesSelector } from '../../../../../metadata/selector';
import { HasuraMetadataV3 } from '../../../../../metadata/types';
import { exportMetadata } from '../../../../../metadata/actions';
import IntrospectionTable from './IntrospectionTable';

interface IntrospectionOptionsComponentProps extends InjectedProps {}

const IntrospectionOptionsComponent: React.FC<IntrospectionOptionsComponentProps> = ({
  metadata,
  allRoles,
  dispatch,
}) => {
  // Fire on component load, so that pro console doesn't crash on undefined metadata
  useEffect(() => {
    dispatch(exportMetadata());
  }, []);

  const disabledRoles =
    metadata?.graphql_schema_introspection?.disabled_for_roles ?? [];

  const tableData = allRoles.map(role => ({
    roleName: role,
    instrospectionIsDisabled: disabledRoles.includes(role),
  }));

  return (
    <SecurityTabs tabName="introspection">
      <IntrospectionTable rows={tableData} />
    </SecurityTabs>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  dispatch,
});
const mapStateToProps = (state: ReduxState) => ({
  metadata: state?.metadata.metadataObject as HasuraMetadataV3,
  allRoles: rolesSelector(state),
});

const IntrospectionOptionsComponentConnector = connect(
  mapStateToProps,
  mapDispatchToProps
);
type InjectedProps = ConnectedProps<
  typeof IntrospectionOptionsComponentConnector
>;

export const IntrospectionOptions = IntrospectionOptionsComponentConnector(
  IntrospectionOptionsComponent
);
