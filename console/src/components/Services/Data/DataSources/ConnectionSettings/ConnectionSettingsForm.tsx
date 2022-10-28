/* eslint-disable no-underscore-dangle */
import { useMetadata } from '@/features/MetadataAPI';
import { isProConsole } from '@/utils/proConsole';
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
  CumulativeMaxConnections,
} from './parts';

export interface ConnectionSettingsFormProps {
  connectionDBState: ConnectDBState;
  connectionDBStateDispatch: Dispatch<ConnectDBActions>;
  isEditState?: boolean;
}

const ConnectionSettingsForm: React.FC<ConnectionSettingsFormProps> = props => {
  const { connectionDBState, isEditState } = props;

  const { data: metadata } = useMetadata();

  const currentDBState = metadata?.metadata?.sources.find(
    d => d.name === connectionDBState?.displayName
  );
  const isMaxConnectionSet =
    currentDBState?.configuration?.connection_info?.pool_settings
      ?.max_connections;

  const formSettings = React.useMemo(
    () => buildFormSettings(connectionDBState.dbType),
    [connectionDBState.dbType]
  );

  if (!formSettings.connectionSettings) return null;

  if (!isEditState) {
    return (
      <FormContainer>
        {!isProConsole(window.__env) && <MaxConnections {...props} />}
        {formSettings.cumulativeMaxConnections && (
          <CumulativeMaxConnections {...props} />
        )}
        <IdleTimeout {...props} />
        {formSettings.retries && <Retries {...props} />}
        {formSettings.pool_timeout && <PoolTimeout {...props} />}
        {formSettings.connection_lifetime && <ConnectionLifetime {...props} />}
        {formSettings.isolation_level && <IsolationLevel {...props} />}
        {formSettings.prepared_statements && <PreparedStatements {...props} />}
        {formSettings.ssl_certificates && <SSLCertificates {...props} />}
      </FormContainer>
    );
  }
  return (
    <FormContainer>
      {isMaxConnectionSet && <MaxConnections {...props} />}
      {formSettings.cumulativeMaxConnections && (
        <CumulativeMaxConnections {...props} />
      )}
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
