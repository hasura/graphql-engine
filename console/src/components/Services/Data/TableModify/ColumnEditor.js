import React, { useEffect } from 'react';
import dataTypes from '../Common/DataTypes';
import { convertListToDictUsingKV } from '../../../../utils/data';
import {
  INTEGER,
  SERIAL,
  BIGINT,
  BIGSERIAL,
  UUID,
  JSONDTYPE,
  JSONB,
  TIMESTAMP,
  TIME,
} from '../../../../constants';

const ColumnEditor = ({
  column,
  onSubmit,
  dispatch,
  columnComment,
  allowRename,
  columnProperties,
  selectedProperties,
  editColumn,
}) => {
  const colName = columnProperties.name;
  useEffect(() => {
    dispatch(editColumn(colName, 'comment', columnComment));
  }, [columnComment]);
  const c = column;
  if (!selectedProperties[colName]) {
    return null;
  }
  const styles = require('./ModifyTable.scss');
  // NOTE: the datatypes is filtered of serial and bigserial where hasuraDatatype === null
  const typeMap = convertListToDictUsingKV(
    'hasuraDatatype',
    'value',
    dataTypes.filter(dataType => dataType.hasuraDatatype)
  );
  const additionalOptions = [];
  let finalDefaultValue = typeMap[columnProperties.type];
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

      case JSONDTYPE:
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

  const updateColumnName = e => {
    dispatch(editColumn(colName, 'name', e.target.value));
  };
  const updateColumnType = e => {
    dispatch(editColumn(colName, 'type', e.target.value));
  };
  const updateColumnDef = e => {
    dispatch(editColumn(colName, 'default', e.target.value));
  };
  const updateColumnComment = e => {
    dispatch(editColumn(colName, 'comment', e.target.value));
  };
  const toggleColumnNullable = e => {
    dispatch(editColumn(colName, 'isNullable', e.target.value === 'true'));
  };
  const toggleColumnUnique = e => {
    dispatch(editColumn(colName, 'isUnique', e.target.value === 'true'));
  };

  return (
    <div className={`${styles.colEditor} container-fluid`}>
      <form className="form-horizontal" onSubmit={onSubmit}>
        {allowRename && (
          <div className={`${styles.display_flex} form-group`}>
            <label className="col-xs-2">Name</label>
            <div className="col-xs-6">
              <input
                className="input-sm form-control"
                value={selectedProperties[colName].name}
                onChange={updateColumnName}
                type="text"
                data-test="edit-col-name"
              />
            </div>
          </div>
        )}
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Type</label>
          <div className="col-xs-6">
            <select
              value={selectedProperties[colName].type}
              onChange={updateColumnType}
              className="input-sm form-control"
              disabled={columnProperties.isPrimaryKey}
            >
              {modifyAlterOptions(columnProperties.type)}
              {additionalOptions}
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Nullable</label>
          <div className="col-xs-6">
            <select
              className="input-sm form-control"
              value={selectedProperties[colName].isNullable}
              onChange={toggleColumnNullable}
              disabled={columnProperties.isPrimaryKey}
              data-test="edit-col-nullable"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Unique</label>
          <div className="col-xs-6">
            <select
              className="input-sm form-control"
              value={selectedProperties[colName].isUnique.toString()}
              onChange={toggleColumnUnique}
              disabled={columnProperties.isPrimaryKey}
              data-test="edit-col-unique"
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Default</label>
          <div className="col-xs-6">
            <input
              className="input-sm form-control"
              value={selectedProperties[colName].default || ''}
              onChange={updateColumnDef}
              type="text"
              disabled={columnProperties.isPrimaryKey}
              data-test="edit-col-default"
            />
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-2">Comment</label>
          <div className="col-xs-6">
            <input
              className="input-sm form-control"
              value={selectedProperties[colName].comment || ''}
              onChange={updateColumnComment}
              type="text"
              data-test="edit-col-comment"
            />
          </div>
        </div>
      </form>
      <div className="row">
        <br />
      </div>
    </div>
  );
};

export default ColumnEditor;
