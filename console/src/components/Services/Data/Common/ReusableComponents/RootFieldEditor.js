import React from 'react';
import styles from '../../../../Common/Common.scss';
import { getRootFieldLabel } from './utils';

const RootFieldEditor = ({
  aliases,
  disabled,
  selectOnChange,
  selectByPkOnChange,
  selectAggOnChange,
  insertOnChange,
  updateOnChange,
  deleteOnChange,
  tableName,
}) => {
  const {
    select,
    select_by_pk: selectByPk,
    select_aggregate: selectAgg,
    insert,
    update,
    delete: _delete,
  } = aliases;

  const [queryExpanded, setQueryExpanded] = React.useState(true);
  const [mutationExpanded, setMutationExpanded] = React.useState(true);

  const getDefaultRootField = rfType => {
    if (rfType.includes('select')) {
      return rfType.replace('select', tableName);
    }
    return `${rfType}_${tableName}`;
  };

  const getRow = (rfType, value, onChange) => (
    <div
      className={`${styles.display_flex} row ${styles.add_mar_bottom_small}`}
    >
      <div className={`${styles.add_mar_right} col-md-3`}>
        {getRootFieldLabel(rfType)}
      </div>
      <div className={'col-md-5'}>
        <input
          type="text"
          value={value || ''}
          placeholder={`${getDefaultRootField(rfType)} (default)`}
          className="form-control"
          onChange={onChange}
          disabled={disabled}
        />
      </div>
    </div>
  );

  const getQuerySection = () => {
    const toggleQueryExpanded = () => setQueryExpanded(!queryExpanded);
    const chevronClassname = `fa fa-chevron-${
      queryExpanded ? 'down' : 'right'
    }`;
    return (
      <div className={`${styles.add_mar_bottom_mid}`}>
        <div
          className={`${styles.add_mar_bottom_small} ${styles.display_flex} ${
            styles.cursorPointer
          }`}
          onClick={toggleQueryExpanded}
        >
          <i className={`${chevronClassname} ${styles.add_mar_right}`} />
          <b> Query and Subscription </b>
        </div>
        {queryExpanded && (
          <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
            {getRow('select', select, selectOnChange)}
            {getRow('select_by_pk', selectByPk, selectByPkOnChange)}
            {getRow('select_aggregate', selectAgg, selectAggOnChange)}
          </div>
        )}
      </div>
    );
  };

  const getMutationSection = () => {
    const toggleMutationExpanded = () => setMutationExpanded(!mutationExpanded);
    const chevronClassname = `fa fa-chevron-${
      mutationExpanded ? 'down' : 'right'
    }`;
    return (
      <div className={`${styles.add_mar_bottom_mid}`}>
        <div
          className={`${styles.add_mar_bottom_small} ${styles.display_flex} ${
            styles.cursorPointer
          }`}
          onClick={toggleMutationExpanded}
        >
          <i className={`${chevronClassname} ${styles.add_mar_right}`} />
          <b> Mutation </b>
        </div>
        {mutationExpanded && (
          <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
            {getRow('insert', insert, insertOnChange)}
            {getRow('update', update, updateOnChange)}
            {getRow('delete', _delete, deleteOnChange)}
          </div>
        )}
      </div>
    );
  };

  return (
    <div>
      <div className={styles.add_mar_bottom_mid}>
        {getQuerySection()}
        {getMutationSection()}
      </div>
    </div>
  );
};

export default RootFieldEditor;
