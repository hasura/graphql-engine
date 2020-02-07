import React, { useState } from 'react';
import styles from '../../../../Common/TableCommon/Table.scss';
// import { } from '../reducer';
import { updateSchemaInfo } from '../../../Data/DataActions';
import {
  getSchemaName,
  getSchemaTables,
  getTableColumnNames,
  getTableName,
  getTrackedTables,
} from '../../../../Common/utils/pgUtils';

const ManualRelationshipSelector = ({
  objectType,
  schemaList,
  allTables,
  dispatch,
  stateCb,
}) => {
  const baseFieldMap = { field: '', refColumn: '' };

  const [relName, setRelName] = useState('');
  const [relType, setRelType] = useState('');
  const [refSchema, setRefSchema] = useState('');
  const [refTable, setRefTable] = useState('');
  const [fieldMappings, setFieldMappings] = useState([baseFieldMap]);

  const _setFieldMappings = newFieldMappings => {
    const lastFieldMap = newFieldMappings[newFieldMappings.length - 1];

    if (!lastFieldMap || (lastFieldMap.field && lastFieldMap.refColumn)) {
      newFieldMappings.push(baseFieldMap);
    }

    setFieldMappings(newFieldMappings);

    stateCb(relName, relType, refSchema, refTable, newFieldMappings);
  };

  const relTypeSelect = () => {
    const handleRelTypeChange = e => {
      setRelType(e.target.value);
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Type:</b>
        </div>
        <select
          value={relType || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-type'}
          onChange={handleRelTypeChange}
        >
          {relType === '' && (
            <option value={''} disabled>
              {'-- relationship type --'}
            </option>
          )}
          <option key="object" value="object">
            Object Relationship
          </option>
          <option key="array" value="array">
            Array Relationship
          </option>
        </select>
      </div>
    );
  };

  const relNameInput = () => {
    const handleRelNameChange = e => {
      setRelName(e.target.value);
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Name:</b>
        </div>
        <input
          onChange={handleRelNameChange}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          placeholder="Enter relationship name"
          // disabled={!relType}
          data-test="rel-name"
          value={relName}
        />
      </div>
    );
  };

  const refSchemaSelect = () => {
    const handleRefSchemaChange = e => {
      setRefSchema(e.target.value);
      setRefTable('');
      _setFieldMappings([]);
      dispatch(updateSchemaInfo({ schemas: [e.target.value] }));
      dispatch(stateCb());
    };

    const orderedSchemaList = schemaList.map(s => getSchemaName(s)).sort();

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Schema:</b>
        </div>
        <select
          value={refSchema || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-schema'}
          onChange={handleRefSchemaChange}
          // disabled={!relType || !relNameInput}
          disabled={!relNameInput}
        >
          {// default unselected option
            refSchema === '' && (
              <option value={''} disabled>
                {'-- reference schema --'}
              </option>
            )}
          {// all reference schema options
            orderedSchemaList.map((rs, j) => (
              <option key={j} value={rs}>
                {rs}
              </option>
            ))}
        </select>
      </div>
    );
  };

  const refTables = {};
  const trackedSchemaTables = getTrackedTables(
    getSchemaTables(allTables, refSchema)
  );

  trackedSchemaTables.forEach(
    ts => (refTables[getTableName(ts)] = getTableColumnNames(ts))
  );

  const refTableSelect = () => {
    const handleRefTableChange = e => {
      setRefTable(e.target.value);
      _setFieldMappings([]);
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={refTable || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-table'}
          onChange={handleRefTableChange}
          disabled={!relNameInput || !refSchema}
          // disabled={!relType || !relNameInput || !refSchema}
        >
          {refTable === '' && (
            <option value={''} disabled>
              {'-- reference table --'}
            </option>
          )}
          {Object.keys(refTables)
            .sort()
            .map((rt, j) => (
              <option key={j} value={rt}>
                {rt}
              </option>
            ))}
        </select>
      </div>
    );
  };

  const columnSelect = () => {
    const selectTitle = !refTable
      ? 'Please select the reference table'
      : undefined;

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`row ${styles.add_mar_bottom_mid}`}>
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>From:</b>
          </div>
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>To:</b>
          </div>
        </div>
        {fieldMappings.map((fieldMap, i) => {
          const field = fieldMap.field;
          const refColum = fieldMap.refColumn;

          // const numColMappings = fieldMappings.length;

          const handleFieldSet = e => {
            const newFieldMap = {
              ...fieldMappings[i],
              field: e.target.value,
            };

            const newFieldMappings = [
              ...fieldMappings.slice(0, i),
              newFieldMap,
              ...fieldMappings.slice(i + 1),
            ];

            _setFieldMappings(newFieldMappings);
          };

          const handleRefColSet = e => {
            const newFieldMap = {
              ...fieldMappings[i],
              refColumn: e.target.value,
            };

            const newFieldMappings = [
              ...fieldMappings.slice(0, i),
              newFieldMap,
              ...fieldMappings.slice(i + 1),
            ];

            _setFieldMappings(newFieldMappings);
          };

          const handleRemoveField = () => {
            const newFieldMappings = [
              ...fieldMappings.slice(0, i),
              ...fieldMappings.slice(i + 1),
            ];

            _setFieldMappings(newFieldMappings);
          };

          let removeIcon;
          if (i + 1 === fieldMappings.length) {
            removeIcon = null;
          } else {
            removeIcon = (
              <i
                className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
                onClick={handleRemoveField}
              />
            );
          }

          const fields = objectType.fields.map(f => f.name);

          return (
            <div
              className={`row ${styles.add_mar_bottom_mid} ${
                styles.display_flex
              }`}
              key={`fk-col-${i}`}
            >
              <div className={`col-sm-4 ${styles.add_mar_right}`}>
                <select
                  className={`form-control ${styles.select} ${
                    styles.wd100Percent
                  }`}
                  value={field}
                  onChange={handleFieldSet}
                  data-test={`manual-relationship-lcol-${i}`}
                  // disabled={!relType || !relNameInput || !refSchema}
                  disabled={!relNameInput || !refSchema}
                  title={selectTitle}
                >
                  {field === '' && (
                    <option value="" disabled>
                      {'-- field --'}
                    </option>
                  )}
                  {fields.map(f => {
                    return (
                      <option key={f} value={f}>
                        {f}
                      </option>
                    );
                  })}
                </select>
              </div>
              <div className={'col-sm-4'}>
                <select
                  className={`form-control ${styles.select} ${
                    styles.wd100Percent
                  }`}
                  value={refColum}
                  onChange={handleRefColSet}
                  disabled={!refTable}
                  title={selectTitle}
                  data-test={`manual-relationship-rcol-${i}`}
                >
                  {refColum === '' && (
                    <option value="" disabled>
                      {'-- ref_column --'}
                    </option>
                  )}
                  {refTables[refTable] &&
                    refTables[refTable].map(rcOpt => {
                      return (
                        <option key={rcOpt} value={rcOpt}>
                          {rcOpt}
                        </option>
                      );
                    })}
                </select>
              </div>
              <div>{removeIcon}</div>
            </div>
          );
        })}
      </div>
    );
  };

  return (
    <div className="form-group">
      {relTypeSelect()}
      {relNameInput()}
      {refSchemaSelect()}
      {refTableSelect()}
      {columnSelect()}
    </div>
  );
};

export default ManualRelationshipSelector;
