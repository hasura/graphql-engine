import React, { useState } from 'react';
import { showErrorNotification } from '../Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import dataTypes from '../Common/DataTypes';
import Button from '../../../Common/Button/Button';
import { addColSql } from '../TableModify/ModifyActions';

import styles from './ModifyTable.scss';

const useColumnEditor = (dispatch, tableName) => {
  const initialState = {
    colName: '',
    colType: '',
    colNull: true,
    colUnique: false,
    colDefault: '',
  };

  const [columnState, setColumnState] = useState(initialState);
  const { colName, colType, colNull, colUnique, colDefault } = columnState;

  const onSubmit = e => {
    e.preventDefault();

    // validate before sending
    if (!gqlPattern.test(colName)) {
      dispatch(
        showErrorNotification(
          gqlColumnErrorNotif[0],
          gqlColumnErrorNotif[1],
          gqlColumnErrorNotif[2],
          gqlColumnErrorNotif[3]
        )
      );
    } else if (colName === '' || colType === '') {
      dispatch(
        showErrorNotification(
          'Error creating column!',
          'Column name/type cannot be empty',
          '',
          {
            custom: 'Column name/type cannot be empty',
          }
        )
      );
    } else {
      dispatch(
        addColSql(
          tableName,
          colName,
          colType,
          colNull,
          colUnique,
          colDefault,
          () => setColumnState(initialState)
        )
      );
    }
  };

  return {
    colName: {
      value: colName,
      onChange: e => {
        setColumnState({ ...columnState, colName: e.target.value });
      },
    },
    colType: {
      value: colType,
      onChange: e => {
        setColumnState({ ...columnState, colType: e.target.value });
      },
    },
    colNull: {
      checked: colNull,
      onChange: e => {
        setColumnState({ ...columnState, colNull: e.target.checked });
      },
    },
    colUnique: {
      checked: colUnique,
      onChange: e => {
        setColumnState({ ...columnState, colUnique: e.target.checked });
      },
    },
    colDefault: {
      value: colDefault,
      onChange: e => {
        setColumnState({ ...columnState, colDefault: e.target.value });
      },
    },
    onSubmit,
  };
};

const alterTypeOptions = dataTypes.map((datatype, index) => (
  <option value={datatype.value} key={index} title={datatype.description}>
    {datatype.name}
  </option>
));

const ColumnCreator = ({ dispatch, tableName }) => {
  const {
    colName,
    colType,
    colNull,
    colUnique,
    colDefault,
    onSubmit,
  } = useColumnEditor(dispatch, tableName);

  return (
    <div className={styles.activeEdit}>
      <form
        className={`form-inline ${styles.display_flex}`}
        onSubmit={onSubmit}
      >
        <input
          placeholder="column name"
          type="text"
          className={`${styles.input} input-sm form-control`}
          data-test="column-name"
          {...colName}
        />
        <select
          className={`${styles.select} input-sm form-control`}
          data-test="data-type"
          {...colType}
        >
          <option disabled value="">
            -- type --
          </option>
          {alterTypeOptions}
        </select>

        <input
          type="checkbox"
          className={`${styles.input} ${styles.nullable} input-sm form-control`}
          data-test="nullable-checkbox"
          {...colNull}
        />
        <label className={styles.nullLabel}>Nullable</label>

        <input
          type="checkbox"
          className={`${styles.input} ${styles.nullable} input-sm form-control`}
          {...colUnique}
          data-test="unique-checkbox"
        />
        <label className={styles.nullLabel}>Unique</label>

        <input
          placeholder="default value"
          type="text"
          className={`${styles.input} ${
            styles.defaultInput
          } input-sm form-control`}
          {...colDefault}
          data-test="default-value"
        />

        <Button
          type="submit"
          color="yellow"
          size="sm"
          data-test="add-column-button"
        >
          + Add column
        </Button>
      </form>
    </div>
  );
};

export default ColumnCreator;
