import React from 'react';
import styles from '../../../../Common/Common.scss';
import { getRootFieldLabel } from './utils';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';

const RootFieldEditor = ({
  rootFields,
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
  } = rootFields;

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

  const getSection = rfType => {
    return (
      <div className={`${styles.add_mar_bottom_mid}`}>
        <CollapsibleToggle
          title={rfType === 'query' ? 'Query and Subscription' : 'Mutation'}
          useDefaultTitleStyle
          isOpen
        >
          {rfType === 'query' && (
            <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
              {getRow('select', select, selectOnChange)}
              {getRow('select_by_pk', selectByPk, selectByPkOnChange)}
              {getRow('select_aggregate', selectAgg, selectAggOnChange)}
            </div>
          )}
          {rfType === 'mutation' && (
            <div className={`${styles.add_pad_left} ${styles.add_pad_right}`}>
              {getRow('insert', insert, insertOnChange)}
              {getRow('update', update, updateOnChange)}
              {getRow('delete', _delete, deleteOnChange)}
            </div>
          )}
        </CollapsibleToggle>
      </div>
    );
  };

  return (
    <div>
      <div className={styles.add_mar_bottom_mid}>
        {getSection('query')}
        {getSection('mutation')}
      </div>
    </div>
  );
};

export default RootFieldEditor;
