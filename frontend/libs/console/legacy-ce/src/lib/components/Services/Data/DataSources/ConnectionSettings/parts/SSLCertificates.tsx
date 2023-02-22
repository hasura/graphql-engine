import { LabeledInput } from '../../../../../Common/LabeledInput';
import { ConnectionSettingsFormProps } from '../ConnectionSettingsForm';
import { SSLModeOptions } from '../../../../../../metadata/types';
import { IconTooltip } from '../../../../../../new-components/Tooltip';
import React from 'react';
import { FaCaretDown, FaCaretRight } from 'react-icons/fa';
import styles from '../../DataSources.module.scss';

export const SSLCertificates: React.VFC<ConnectionSettingsFormProps> = ({
  connectionDBState,
  connectionDBStateDispatch,
}) => {
  const [showCertSettings, setShowCertSettings] = React.useState(false);

  const handleCertificateSettingsClick = () =>
    setShowCertSettings(!showCertSettings);

  return (
    <div className={styles.add_mar_top}>
      <div
        onClick={handleCertificateSettingsClick}
        className={styles.connection_settings_header}
      >
        {showCertSettings ? <FaCaretDown /> : <FaCaretRight />}
        {`  SSL Certificates Settings`}
      </div>
      <div className={styles.text_muted}>
        Certificates will be loaded from{' '}
        <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
          environment variables
        </a>
      </div>
      {showCertSettings && (
        <div className="mt-xs">
          <div className="mb-xs">
            <label className="flex items-center gap-1">
              <b>SSL Mode</b>
              <IconTooltip message="SSL certificate verification mode" />
            </label>
            <select
              className="form-control"
              onChange={e =>
                connectionDBStateDispatch({
                  type: 'UPDATE_SSL_MODE',
                  data: (e.target.value as SSLModeOptions) || undefined,
                })
              }
              value={connectionDBState.sslConfiguration?.sslmode}
            >
              <option value="">--</option>
              <option value="disable">disable</option>
              <option value="verify-ca">verify-ca</option>
              <option value="verify-full">verify-full</option>
            </select>
          </div>
          <LabeledInput
            label="SSL Root Certificate"
            type="text"
            placeholder="SSL_ROOT_CERT"
            tooltipText="Environment variable that stores trusted certificate authorities"
            value={
              connectionDBState.sslConfiguration?.sslrootcert?.from_env ??
              undefined
            }
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_SSL_ROOT_CERT',
                data: e.target.value,
              })
            }
          />
          <LabeledInput
            label="SSL Certificate"
            type="text"
            placeholder="SSL_CERT"
            tooltipText="Environment variable that stores the client certificate (Optional)"
            value={
              connectionDBState.sslConfiguration?.sslcert?.from_env ?? undefined
            }
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_SSL_CERT',
                data: e.target.value,
              })
            }
          />
          <LabeledInput
            label="SSL Key"
            type="text"
            placeholder="SSL_KEY"
            tooltipText="Environment variable that stores the client private key (Optional)"
            value={
              connectionDBState.sslConfiguration?.sslkey?.from_env ?? undefined
            }
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_SSL_KEY',
                data: e.target.value,
              })
            }
          />
          <LabeledInput
            label="SSL Password"
            type="text"
            className="form-control"
            placeholder="SSL_PASSWORD"
            tooltipText="Environment variable that stores the password if the client private key is encrypted (Optional)"
            value={
              connectionDBState.sslConfiguration?.sslpassword?.from_env ??
              undefined
            }
            onChange={e =>
              connectionDBStateDispatch({
                type: 'UPDATE_SSL_PASSWORD',
                data: e.target.value,
              })
            }
          />
        </div>
      )}
    </div>
  );
};
