import React from 'react';

import { getRootFieldLabel } from './utils';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import { Box, Flex } from '../../../../UIKit/atoms';
import styles from '../../../../Common/Common.scss';

const RootFieldEditor = ({
  rootFields,
  disabled,
  selectOnChange,
  selectByPkOnChange,
  selectAggOnChange,
  insertOnChange,
  insertOneOnChange,
  updateOnChange,
  updateByPkOnChange,
  deleteOnChange,
  deleteByPkOnChange,
  tableName,
}) => {
  const {
    select,
    select_by_pk: selectByPk,
    select_aggregate: selectAgg,
    insert,
    insert_one: insertOne,
    update,
    update_by_pk: updateByPk,
    delete: _delete,
    delete_by_pk: deleteByPk,
  } = rootFields;

  const getDefaultRootField = rfType => {
    if (rfType.includes('select')) {
      return rfType.replace('select', tableName);
    }
    return `${rfType}_${tableName}`;
  };

  const getRow = (rfType, value, onChange) => (
    <Flex mb="5px">
      <Box mr="20px" className="col-md-3">
        {getRootFieldLabel(rfType)}
      </Box>
      <Box className="col-md-5">
        <input
          type="text"
          value={value || ''}
          placeholder={`${getDefaultRootField(rfType)} (default)`}
          className="form-control"
          onChange={onChange}
          disabled={disabled}
        />
      </Box>
    </Flex>
  );

  const getSection = rfType => {
    return (
      <Box mb="10px">
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
              {getRow('insert_one', insertOne, insertOneOnChange)}
              {getRow('update', update, updateOnChange)}
              {getRow('update_by_pk', updateByPk, updateByPkOnChange)}
              {getRow('delete', _delete, deleteOnChange)}
              {getRow('delete_by_pk', deleteByPk, deleteByPkOnChange)}
            </div>
          )}
        </CollapsibleToggle>
      </Box>
    );
  };

  return (
    <Box mb="10px">
      {getSection('query')}
      {getSection('mutation')}
    </Box>
  );
};

export default RootFieldEditor;
