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
  JSONDTYPE,
  JSONB,
  TIMESTAMP,
  TIME,
} from '../../../../constants';
import Button from '../../../Common/Button/Button';

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
  allowRename,
  columnProperties,
}) => {
  //  eslint-disable-line no-unused-vars
  const c = column;
  const styles = require('./ModifyTable.scss');
  let [iname, inullable, iunique, idefault, icomment, itype] = [
    null,
    null,
    null,
    null,
    null,
    null,
  ];
  // NOTE: the datatypes is filtered of serial and bigserial where hasuraDatatype === null
  const tableSchema = allSchemas.find(t => t.table_name === tableName);
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

  return (
    <div className={`${styles.colEditor} container-fluid`}>
      <form
        className="form-horizontal"
        onSubmit={e => {
          e.preventDefault();
          console.log(itype.value);
          console.log(inullable.value);
          console.log(iunique.value);
          console.log(icomment.value);
          onSubmit(
            itype.value,
            inullable.value,
            iunique.value,
            idefault.value,
            icomment.value,
            column,
            allowRename ? iname.value : null
          );
        }}
      >
        {allowRename && (
          <div className={`${styles.display_flex} form-group`}>
            <label className="col-xs-3 text-right">Name</label>
            <div className="col-xs-6">
              <input
                ref={n => (iname = n)}
                className="input-sm form-control"
                defaultValue={columnProperties.name}
                type="text"
                data-test="edit-col-name"
              />
            </div>
          </div>
        )}
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Type</label>
          <div className="col-xs-6">
            <select
              ref={n => (itype = n)}
              className="input-sm form-control"
              defaultValue={finalDefaultValue}
              disabled={columnProperties.isPrimaryKey}
            >
              {modifyAlterOptions(columnProperties.type)}
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
              defaultValue={columnProperties.isNullable.toString()}
              disabled={columnProperties.isPrimaryKey}
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
              defaultValue={columnProperties.isUnique.toString()}
              disabled={columnProperties.isPrimaryKey}
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
              defaultValue={columnProperties.default || ''}
              type="text"
              disabled={columnProperties.isPrimaryKey}
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
      </form>
      <div className="row">
        <br />
      </div>
    </div>
  );
};

export default ColumnEditor;
