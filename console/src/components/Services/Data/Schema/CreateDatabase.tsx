import React, { useState } from 'react';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import SearchableSelect from '../../../Common/SearchableSelect/SearchableSelect';
import styles from '../../../Common/Common.scss';
import { drivers } from '../../../../dataSources';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';

const customSelectBoxStyles = {
  control: {
    height: '34px',
    minHeight: '34px !important',
  },
  container: {
    width: '186px',
    height: '34px',
    minHeight: '34px !important',
  },
  dropdownIndicator: {
    padding: '5px',
  },
  placeholder: {
    top: '44%',
    fontSize: '12px',
  },
  singleValue: {
    fontSize: '12px',
    top: '44%',
    color: '#555555',
  },
};

type CreateDatabaseProps = {
  onSubmit(): void;
};
const CreateDatabase = ({ onSubmit }: CreateDatabaseProps) => {
  const [databaseName, setDatabaseName] = useState('');
  const [databaseType, setDatabaseType] = useState('');
  const [databaseUrl, setDatabaseUrl] = useState('');
  const [urlType, setUrlType] = useState<'static' | 'from-env'>('static');

  const expandedContent = () => (
    <div>
      <form className={`form-inline ${styles.display_flex}`}>
        <span className={styles.add_mar_right_mid} style={{ height: '34px' }}>
          <SearchableSelect
            options={drivers}
            onChange={(opt: any) =>
              setDatabaseType(typeof opt === 'string' ? opt : opt.value)
            }
            value={databaseType}
            bsClass="modify_select"
            styleOverrides={customSelectBoxStyles}
            filterOption="prefix"
            placeholder="database_type"
          />
        </span>
        <input
          placeholder="database name"
          type="text"
          className={`${styles.add_mar_right_mid} ${styles.wd100Percent} input-sm form-control`}
          value={databaseName}
          onChange={e => setDatabaseName(e.target.value)}
          style={{ height: '34px' }}
        />
        <div className={styles.wd100Percent}>
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
            bsClass={styles.dropdown_button}
            inputVal={databaseUrl}
            inputPlaceHolder={
              urlType === 'from-env' ? 'HEADER_FROM_ENV' : 'value'
            }
          />
        </div>
      </form>
    </div>
  );
  return (
    <ExpandableEditor
      editorExpanded={expandedContent}
      property="add-new-database"
      expandButtonText="Add database"
      saveFunc={onSubmit}
      isCollapsable
    />
  );
};
export default CreateDatabase;
