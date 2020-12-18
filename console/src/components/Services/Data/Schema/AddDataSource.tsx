import React, { useState } from 'react';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import styles from '../../../Common/Common.scss';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import { DataSource } from '../../../../metadata/types';

// const customSelectBoxStyles = {
//   control: {
//     height: '34px',
//     minHeight: '34px !important',
//   },
//   container: {
//     width: '156px',
//     height: '34px',
//     minHeight: '34px !important',
//   },
//   dropdownIndicator: {
//     padding: '5px',
//   },
//   placeholder: {
//     top: '50%',
//     fontSize: '12px',
//   },
//   singleValue: {
//     fontSize: '12px',
//     top: '44%',
//     color: '#555555',
//   },
// };

type AddDataSourceProps = {
  onSubmit(data: DataSource, successCallback: () => void): void;
};
const AddDataSource = ({ onSubmit }: AddDataSourceProps) => {
  const [databaseName, setDatabaseName] = useState('');
  const [databaseUrl, setDatabaseUrl] = useState('');
  const [urlType, setUrlType] = useState<'static' | 'from-env'>('static');
  const [retryConf, setRetryConf] = useState<{
    max_connections?: number;
    idle_timeout?: number;
    retries?: number;
  }>({});
  const [settingsOpen, setSettingsOpen] = useState(false);

  const handleSubmit = () => {
    onSubmit(
      {
        name: databaseName.trim(),
        driver: 'postgres',
        fromEnv: urlType === 'from-env',
        connection_pool_settings: retryConf,
        url: databaseUrl.trim(),
      },
      () => {
        setDatabaseUrl('');
        setDatabaseName('');
        setUrlType('static');
        setRetryConf({});
        setSettingsOpen(false);
      }
    );
  };

  const expandedContent = () => (
    <div style={{ width: '100%' }}>
      <form className={`form-inline ${styles.display_flex}`}>
        {/* <span className={styles.add_mar_right_mid}>
          <SearchableSelect
            options={drivers}
            onChange={(opt: any) =>
              setDatabaseType(typeof opt === 'string' ? opt : opt.value)
            }
            value={databaseType || ''}
            bsClass="modify_select"
            styleOverrides={customSelectBoxStyles}
            filterOption="prefix"
            placeholder="database type"
          />
        </span> */}
        <input
          placeholder="database name"
          type="text"
          className={`${styles.add_mar_right_mid} ${styles.wd100Percent} input-sm form-control`}
          value={databaseName}
          onChange={e => setDatabaseName(e.target.value)}
          style={{ height: '34px' }}
        />
        <DropdownButton
          dropdownOptions={[
            { display_text: 'Value', value: 'static' },
            { display_text: 'From env var', value: 'from-env' },
          ]}
          title={urlType === 'from-env' ? 'From env var' : 'Value'}
          dataKey={urlType === 'from-env' ? 'env' : 'static'}
          onButtonChange={(e: React.BaseSyntheticEvent) =>
            setUrlType(e.target.getAttribute('value'))
          }
          onInputChange={e => setDatabaseUrl(e.target.value)}
          required
          inputVal={databaseUrl}
          inputPlaceHolder="database url"
          testId="database-url"
          inputStyle={{ width: '300px', fontSize: '12px' }}
        />
      </form>
      <div style={{ marginTop: '20px' }}>
        <CollapsibleToggle
          title="Connection settings"
          isOpen={settingsOpen}
          toggleHandler={() => setSettingsOpen(prev => !prev)}
          useDefaultTitleStyle
        >
          <div style={{ paddingTop: '10px' }}>
            <b>Max connections: </b>
            <input
              placeholder="default: 50"
              type="number"
              className={`${styles.add_mar_right_mid} input-sm form-control`}
              value={retryConf.max_connections || ''}
              onChange={e => {
                e.persist();
                setRetryConf(prev => ({
                  ...prev,
                  max_connections: parseInt(e.target.value, 10),
                }));
              }}
              style={{ height: '34px', width: '200px', marginTop: '8px' }}
            />
          </div>
          <div style={{ paddingTop: '16px' }}>
            <b>Connection idle timeout: </b>
            <input
              placeholder="default: 180"
              type="number"
              className={`${styles.add_mar_right_mid} input-sm form-control`}
              value={retryConf.idle_timeout || ''}
              onChange={e => {
                e.persist();
                setRetryConf(prev => ({
                  ...prev,
                  idle_timeout: parseInt(e.target.value, 10),
                }));
              }}
              style={{ height: '34px', width: '200px', marginTop: '8px' }}
            />
          </div>
          <div style={{ paddingTop: '16px' }}>
            <b>Retries: </b>
            <input
              placeholder="default: 1"
              type="number"
              className={`${styles.add_mar_right_mid} input-sm form-control`}
              value={retryConf.retries || ''}
              onChange={e => {
                e.persist();
                setRetryConf(prev => ({
                  ...prev,
                  retries: parseInt(e.target.value, 10),
                }));
              }}
              style={{ height: '34px', width: '200px', marginTop: '8px' }}
            />
          </div>
        </CollapsibleToggle>
      </div>
    </div>
  );
  return (
    <ExpandableEditor
      editorExpanded={expandedContent}
      property="add-new-database"
      expandButtonText="Add database"
      saveFunc={handleSubmit}
      isCollapsable
    />
  );
};
export default AddDataSource;
