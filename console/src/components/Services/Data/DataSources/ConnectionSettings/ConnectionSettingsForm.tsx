import React, { Dispatch } from 'react';
import { ConnectDBActions, ConnectDBState } from '../state';
import { buildFormSettings } from './buildFormSettings';
import {
  FormContainer,
  ConnectionLifetime,
  IdleTimeout,
  IsolationLevel,
  MaxConnections,
  PoolTimeout,
  PreparedStatements,
  Retries,
  SSLCertificates,
} from './parts';

export interface ConnectionSettingsFormProps {
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
}

const ConnectionSettingsForm: React.FC<ConnectionSettingsFormProps> = props => {
  const { connectionDBState } = props;

  const formSettings = React.useMemo(
    () => buildFormSettings(connectionDBState.dbType),
    [connectionDBState.dbType]
  );

  if (!formSettings.connectionSettings) return null;

  return (
    <FormContainer>
      <MaxConnections {...props} />
      {/* {formSettings.cumulativeMaxConnections && (
        <CumulativeMaxConnections {...props} />
      )} */}
      <IdleTimeout {...props} />
      {formSettings.retries && <Retries {...props} />}
      {formSettings.pool_timeout && <PoolTimeout {...props} />}
      {formSettings.connection_lifetime && <ConnectionLifetime {...props} />}
      {formSettings.isolation_level && <IsolationLevel {...props} />}
      {formSettings.prepared_statements && <PreparedStatements {...props} />}
      {formSettings.ssl_certificates && <SSLCertificates {...props} />}
    </FormContainer>
  );
};

export default ConnectionSettingsForm;
