import React, { useState } from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import SearchableSelect from '../../../Common/SearchableSelect/SearchableSelect';
import styles from '../../../Common/Common.scss';
import { drivers } from '../../../../dataSources';

const customSelectBoxStyles = {
  container: {
    width: '186px',
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

  const expandedContent = () => (
    <div>
      <form className={`form-inline ${styles.display_flex}`}>
        <span className={styles.add_mar_right_mid} data-test="db-type-0">
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
        />
        <input
          placeholder="database url"
          type="url"
          className={`${styles.add_mar_right_mid} ${styles.wd100Percent} input-sm form-control`}
          value={databaseUrl}
          onChange={e => setDatabaseUrl(e.target.value)}
        />
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
