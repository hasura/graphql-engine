import React from 'react';
import { connect, ConnectedProps, Provider } from 'react-redux';

import { SecurityTabs } from './SecurityTabs';
import LimitsTable from './LimitsTable';
import { Dispatch, ReduxState } from '../../../../types';
import { rolesSelector } from '../../../../metadata/selector';
// import { exportMetadata } from '../../../../metadata/actions';
import { apiLimitStore } from './state';

interface securitySettingsComponentProps extends InjectedProps {}

const ApiLimitsComponent: React.FC<securitySettingsComponentProps> = ({
  metadata,
  allRoles,
  dispatch,
}) => {
  // useEffect(() => {
  //   dispatch(exportMetadata());
  // }, [dispatch]);

  const headers = [
    'Role',
    'Depth Limit',
    'Node Limit',
    'Rate Limit (RPM)',
    // 'Operation Timeout (Seconds)',
  ];
  const keys = [
    'role',
    'depth_limit',
    'node_limit',
    'rate_limit',
    // 'operation_timeout',
  ];
  const roles = allRoles;
  return (
    <SecurityTabs tabName="api_limits">
      <Provider store={apiLimitStore}>
        <LimitsTable
          headers={headers}
          keys={keys}
          roles={roles}
          dispatch={dispatch}
          metadata={metadata}
        />
      </Provider>
    </SecurityTabs>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  dispatch,
});
const mapStateToProps = (state: ReduxState) => ({
  metadata: state?.metadata.metadataObject,
  allRoles: rolesSelector(state),
});

const apiLimitsComponentConnector = connect(
  mapStateToProps,
  mapDispatchToProps
);
type InjectedProps = ConnectedProps<typeof apiLimitsComponentConnector>;

export const ApiLimits = apiLimitsComponentConnector(ApiLimitsComponent);
