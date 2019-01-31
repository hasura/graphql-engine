import React from 'react';
import { Link } from 'react-router';
import {
  fkRefTableChange,
  fkRColChange,
  toggleFKCheckBox,
  isColumnUnique,
  deleteConstraintSql,
} from '../TableModify/ModifyActions';
import dataTypes from '../Common/DataTypes';
import { convertListToDictUsingKV } from '../../../../utils/data';
import {
  INTEGER,
  SERIAL,
  BIGINT,
  BIGSERIAL,
  UUID,
  JSON,
  JSONB,
  TIMESTAMP,
  TIME,
} from '../../../../constants';
import Button from '../../Layout/Button/Button';

const appPrefix = '/data';

const ColumnEditor = ({
  column,
  onSubmit,
  onDelete,
  allSchemas,
  fkAdd,
  tableName,
  dispatch,
  currentSchema,
  columnComment,
}) => {
  //  eslint-disable-line no-unused-vars
  const c = column;
  const styles = require('./Modify.scss');
  let [inullable, iunique, idefault, icomment, itype] = [
    null,
    null,
    null,
    null,
    null,
  ];
  // NOTE: the datatypes is filtered of serial and bigserial where hasuraDatatype === null
  const refTable = fkAdd.refTable;
  const tableSchema = allSchemas.find(t => t.table_name === tableName);
  const rcol = fkAdd.rcol;
  const typeMap = convertListToDictUsingKV(
    'hasuraDatatype',
    'value',
    dataTypes.filter(dataType => dataType.hasuraDatatype)
  );
  const refSchema = allSchemas.find(t => t.table_name === refTable);
  // const allTableNamesExceptCurrent = allSchemas.filter(t => t.table_name !== tableName);
  const allTableNames = allSchemas.map(t => t.table_name);
  allTableNames.sort();

  const refColumnNames = refSchema
    ? refSchema.columns.map(col => col.column_name)
    : [];
  refColumnNames.sort();
  const onFKRefTableChange = e => {
    dispatch(fkRefTableChange(e.target.value));
  };
  const onFKRefColumnChange = e => {
    dispatch(fkRColChange(e.target.value));
  };
  const checkExistingForeignKey = () => {
    const numFk = tableSchema.foreign_key_constraints.length;
    let fkName = '';
    const onDeleteFK = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(deleteConstraintSql(tableName, fkName));
      }
    };
    if (numFk > 0) {
      for (let i = 0; i < numFk; i++) {
        const fk = tableSchema.foreign_key_constraints[i];
        if (
          Object.keys(fk.column_mapping).toString() === c.column_name.toString()
        ) {
          fkName = fk.constraint_name;
          return (
            <div className={`${styles.display_flex} form-group`}>
              <label className="col-xs-3 text-right">Foreign Key</label>
              <div className="col-xs-9">
                <h5>
                  <span>{fk.ref_table} :: </span>
                  <span className={styles.add_mar_right}>
                    {Object.keys(fk.column_mapping)
                      .map(l => fk.column_mapping[l])
                      .join(',')}
                  </span>
                  <Link
                    to={`${appPrefix}/schema/${currentSchema}/tables/${tableName}/relationships`}
                  >
                    <Button
                      color="white"
                      size="sm"
                      type="button"
                      data-test="add-rel-mod"
                    >
                      +Add relationship
                    </Button>
                  </Link>
                  &nbsp;
                  <Button
                    color="red"
                    size="sm"
                    onClick={onDeleteFK}
                    data-test="remove-constraint-button"
                  >
                    {' '}
                    Remove Constraint{' '}
                  </Button>{' '}
                  &nbsp;
                </h5>
              </div>
            </div>
          );
        }
      }
    }
    return (
      <div className={`${styles.display_flex} form-group`}>
        <label className="col-xs-3 text-right">
          <input
            type="checkbox"
            checked={fkAdd.fkCheckBox}
            onChange={e => {
              dispatch(toggleFKCheckBox(e.target.checked));
            }}
            value="ForeignKey"
            data-test="foreign-key-checkbox"
          />{' '}
          Foreign Key
        </label>
        <div className="col-xs-6">
          <select
            className={`${styles.fkSelect} ${styles.fkInEdit} ${
              styles.fkInEditLeft
            } input-sm form-control`}
            disabled={fkAdd.fkCheckBox === false}
            value={refTable}
            onChange={onFKRefTableChange}
            data-test="ref-table"
          >
            <option disabled value="">
              Reference table
            </option>
            {allTableNames.map((tName, i) => (
              <option key={i} value={tName}>
                {tName}
              </option>
            ))}
          </select>
          <select
            className={`${styles.fkSelect} ${
              styles.fkInEdit
            } input-sm form-control`}
            disabled={fkAdd.fkCheckBox === false}
            value={rcol}
            onChange={onFKRefColumnChange}
            data-test="ref-col"
          >
            <option disabled value="">
              Reference column
            </option>
            {refColumnNames.map((co, i) => (
              <option key={i} value={co}>
                {co}
              </option>
            ))}
          </select>
        </div>
      </div>
    );
  };
  let isPrimaryKey = false;
  const isUnique = isColumnUnique(tableSchema, c.column_name);

  if (
    tableSchema.primary_key &&
    tableSchema.primary_key.columns.includes(c.column_name)
  ) {
    isPrimaryKey = true;
  }

  const additionalOptions = [];
  let finalDefaultValue = typeMap[c.data_type];
  if (!typeMap[c.data_type]) {
    finalDefaultValue = c.data_type;
    additionalOptions.push(
      <option value={finalDefaultValue} key={finalDefaultValue}>
        {c.data_type}
      </option>
    );
  }

  const generateAlterOptions = datatypeOptions => {
    return dataTypes.map(datatype => {
      if (datatypeOptions.includes(datatype.value)) {
        return (
          <option
            value={datatype.value}
            key={datatype.name}
            title={datatype.description}
          >
            {datatype.name}
          </option>
        );
      }
    });
  };

  const modifyAlterOptions = columntype => {
    const integerOptions = [
      'integer',
      'serial',
      'bigint',
      'bigserial',
      'numeric',
      'text',
    ];
    const bigintOptions = ['bigint', 'bigserial', 'text', 'numeric'];
    const uuidOptions = ['uuid', 'text'];
    const jsonOptions = ['json', 'jsonb', 'text'];
    const timestampOptions = ['timestamptz', 'text'];
    const timeOptions = ['timetz', 'text'];
    switch (columntype) {
      case INTEGER:
        return generateAlterOptions(integerOptions);

      case SERIAL:
        return generateAlterOptions(integerOptions);

      case BIGINT:
        return generateAlterOptions(bigintOptions);

      case BIGSERIAL:
        return generateAlterOptions(bigintOptions);

      case UUID:
        return generateAlterOptions(uuidOptions);

      case JSON:
        return generateAlterOptions(jsonOptions);

      case JSONB:
        return generateAlterOptions(jsonOptions);

      case TIMESTAMP:
        return generateAlterOptions(timestampOptions);

      case TIME:
        return generateAlterOptions(timeOptions);

      default:
        return generateAlterOptions([columntype, 'text']);
    }
  };

  return (
    <div className={`${styles.colEditor} container-fluid`}>
      <form
        className="form-horizontal"
        onSubmit={e => {
          e.preventDefault();
          onSubmit(
            itype.value,
            inullable.value,
            iunique.value,
            idefault.value,
            icomment.value,
            column
          );
        }}
      >
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Type</label>
          <div className="col-xs-6">
            <select
              ref={n => (itype = n)}
              className="input-sm form-control"
              defaultValue={finalDefaultValue}
              disabled={isPrimaryKey}
            >
              {modifyAlterOptions(column.data_type)}
              {additionalOptions}
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Nullable</label>
          <div className="col-xs-6">
            <select
              ref={n => (inullable = n)}
              className="input-sm form-control"
              defaultValue={c.is_nullable === 'NO' ? 'false' : 'true'}
              disabled={isPrimaryKey}
              data-test="edit-col-nullable"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Unique</label>
          <div className="col-xs-6">
            <select
              ref={n => (iunique = n)}
              className="input-sm form-control"
              defaultValue={isUnique.toString()}
              disabled={isPrimaryKey}
              data-test="edit-col-unique"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Default</label>
          <div className="col-xs-6">
            <input
              ref={n => (idefault = n)}
              className="input-sm form-control"
              defaultValue={c.column_default ? c.column_default : null}
              type="text"
              disabled={isPrimaryKey}
              data-test="edit-col-default"
            />
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Comment</label>
          <div className="col-xs-6">
            <input
              ref={n => (icomment = n)}
              className="input-sm form-control"
              defaultValue={columnComment ? columnComment.result[1] : null}
              type="text"
              data-test="edit-col-comment"
            />
          </div>
        </div>
        {checkExistingForeignKey()}
        <div className="row">
          <Button
            type="submit"
            color="yellow"
            className={styles.button_mar_right}
            size="sm"
            data-test="save-button"
          >
            Save
          </Button>
          {!isPrimaryKey ? (
            <Button
              type="submit"
              color="red"
              size="sm"
              onClick={e => {
                e.preventDefault();
                onDelete();
              }}
              data-test="remove-button"
            >
              Remove
            </Button>
          ) : null}
        </div>
      </form>
      <div className="row">
        <br />
      </div>
    </div>
  );
};

export default ColumnEditor;
