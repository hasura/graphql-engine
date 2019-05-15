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
  NUMERIC,
  TEXT,
} from '../utils';

const ColumnEditor = ({
  onSubmit,
  dispatch,
  columnComment,
  allowRename,
  columnProperties,
  selectedProperties,
  editColumn,
}) => {
  const colName = columnProperties.name;

  if (!selectedProperties[colName]) {
    return null;
  }

  const styles = require('./ModifyTable.scss');

  useEffect(() => {
    dispatch(editColumn(colName, 'comment', columnComment));
  }, [columnComment]);

  // filter the datatypes where hasuraDatatype === null
  const typeMap = convertListToDictUsingKV(
    'hasuraDatatype',
    'value',
    dataTypes.filter(dataType => dataType.hasuraDatatype)
  );

  const getAlternateTypeOptions = columntype => {
    const generateOptions = datatypeOptions => {
      const options = [];

      dataTypes.forEach(datatype => {
        if (datatypeOptions.includes(datatype.value)) {
          options.push(
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

      let finalDefaultValue = typeMap[columnProperties.type];
      if (!finalDefaultValue) {
        finalDefaultValue = columnProperties.type;
        options.push(
          <option value={finalDefaultValue} key={finalDefaultValue}>
            {finalDefaultValue}
          </option>
        );
      }

      return options;
    };

    const integerOptions = [INTEGER, SERIAL, BIGINT, BIGSERIAL, NUMERIC, TEXT];
    const bigintOptions = [BIGINT, BIGSERIAL, NUMERIC, TEXT];
    const uuidOptions = [UUID, TEXT];
    const jsonOptions = [JSON, JSONB, TEXT];
    const timestampOptions = [TIMESTAMP, TEXT];
    const timeOptions = [TIME, TEXT];

    switch (columntype) {
      case INTEGER:
        return generateOptions(integerOptions);

      case SERIAL:
        return generateOptions(integerOptions);

      case BIGINT:
        return generateOptions(bigintOptions);

      case BIGSERIAL:
        return generateOptions(bigintOptions);

      case UUID:
        return generateOptions(uuidOptions);

      case JSONDTYPE:
        return generateOptions(jsonOptions);

      case JSONB:
        return generateOptions(jsonOptions);

      case TIMESTAMP:
        return generateOptions(timestampOptions);

      case TIME:
        return generateOptions(timeOptions);

      default:
        return generateOptions([columntype, TEXT]);
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
              disabled={columnProperties.pkConstraint}
            >
              {getAlternateTypeOptions(columnProperties.type)}
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
              disabled={columnProperties.pkConstraint}
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
              value={selectedProperties[colName].isUnique}
              onChange={toggleColumnUnique}
              disabled={columnProperties.pkConstraint}
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
              disabled={columnProperties.pkConstraint}
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
