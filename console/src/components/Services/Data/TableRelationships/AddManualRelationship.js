import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';
import { ordinalColSort } from '../utils';
import {
  manualRelRTableChanged,
  manualRelTypeChanged,
  setManualRelAdd,
  manualRelNameChanged,
  addRelViewMigrate,
  resetManualRelationshipForm,
  manualRelRSchemaChanged,
} from './Actions';
import { updateSchemaInfo } from '../DataActions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

const ManualRelationshipSelector = ({
  tableSchema,
  schemaList,
  refTables,
  relAdd,
  orderedColumns,
  dispatch,
}) => {
  const relTypeSelect = () => {
    const dispatchSetRelType = event => {
      dispatch(manualRelTypeChanged(event.target.value));
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Type:</b>
        </div>
        <select
          value={relAdd.relType || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-type'}
          onChange={dispatchSetRelType}
        >
          {relAdd.relType === '' && (
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

  const relName = () => {
    const dispatchSetRelName = event => {
      dispatch(manualRelNameChanged(event.target.value));
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Name:</b>
        </div>
        <input
          onChange={dispatchSetRelName}
          className="form-control"
          placeholder="Enter relationship name"
          disabled={!relAdd.relType}
          data-test="rel-name"
        />
      </div>
    );
  };

  const refSchemaSelect = () => {
    const dispatchSetRefSchema = event => {
      dispatch(manualRelRSchemaChanged(event.target.value));
      if (tableSchema.table_schema !== event.target.value) {
        dispatch(updateSchemaInfo({ schemas: [event.target.value] }));
      }
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Schema:</b>
        </div>
        <select
          value={relAdd.rSchema || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-schema'}
          onChange={dispatchSetRefSchema}
          disabled={!relAdd.relType || !relAdd.relName}
        >
          {// default unselected option
            relAdd.rSchema === '' && (
              <option value={''} disabled>
                {'-- reference schema --'}
              </option>
            )}
          {// all reference schema options
            schemaList.map((rs, j) => (
              <option key={j} value={rs}>
                {rs}
              </option>
            ))}
        </select>
      </div>
    );
  };

  const refTableSelect = () => {
    const dispatchSetRefTable = event => {
      dispatch(manualRelRTableChanged(event.target.value));
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={relAdd.rTable || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-table'}
          onChange={dispatchSetRefTable}
          disabled={!relAdd.relType || !relAdd.relName || !relAdd.rSchema}
        >
          {relAdd.rTable === '' && (
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
    const selectTitle = !relAdd.rTable
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
        {relAdd.colMappings.map((colMap, _i) => {
          // from column
          const lc = colMap.column;

          // to column
          const rc = colMap.refColumn;

          const numColMappings = relAdd.colMappings.length;

          const dispatchSetCols = (key, value) => {
            const newRelAdd = JSON.parse(JSON.stringify(relAdd));
            newRelAdd.colMappings[_i][key] = value;
            if (
              newRelAdd.colMappings[numColMappings - 1].column &&
              newRelAdd.colMappings[numColMappings - 1].refColumn
            ) {
              newRelAdd.colMappings.push({ column: '', refColumn: '' });
            }
            dispatch(setManualRelAdd(newRelAdd));
          };

          const dispatchSetLcol = event => {
            dispatchSetCols('column', event.target.value);
          };

          const dispatchSetRcol = event => {
            dispatchSetCols('refColumn', event.target.value);
          };

          const dispatchRemoveCol = () => {
            const newRelAdd = JSON.parse(JSON.stringify(relAdd));
            const newColMapping = [
              ...relAdd.colMappings.slice(0, _i),
              ...relAdd.colMappings.slice(_i + 1),
            ];
            newRelAdd.colMappings = newColMapping;
            dispatch(setManualRelAdd(newRelAdd));
          };

          let removeIcon;
          if (_i + 1 === relAdd.colMappings.length) {
            removeIcon = null;
          } else {
            removeIcon = (
              <i
                className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
                onClick={dispatchRemoveCol}
              />
            );
          }

          return (
            <div
              className={`row ${styles.add_mar_bottom_mid} ${
                styles.display_flex
              }`}
              key={`fk-col-${_i}`}
            >
              <div className={`col-sm-4 ${styles.add_mar_right}`}>
                <select
                  className={`form-control ${styles.select} ${
                    styles.wd100Percent
                  }`}
                  value={lc}
                  onChange={dispatchSetLcol}
                  data-test={`manual-relationship-lcol-${_i}`}
                  disabled={
                    !relAdd.relType || !relAdd.relName || !relAdd.rSchema
                  }
                  title={selectTitle}
                >
                  {lc === '' && (
                    <option value="" disabled>
                      {'-- column --'}
                    </option>
                  )}
                  {orderedColumns.map(oc => {
                    return (
                      <option key={oc.name} value={oc.name}>
                        {oc.name}
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
                  value={rc}
                  onChange={dispatchSetRcol}
                  disabled={!relAdd.rTable}
                  title={selectTitle}
                  data-test={`manual-relationship-rcol-${_i}`}
                >
                  {rc === '' && (
                    <option value="" disabled>
                      {'-- ref_column --'}
                    </option>
                  )}
                  {refTables[relAdd.rTable] &&
                    refTables[relAdd.rTable].map(rcOpt => {
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
      {relName()}
      {refSchemaSelect()}
      {refTableSelect()}
      {columnSelect()}
    </div>
  );
};

const AddManualRelationship = ({
  tableSchema,
  allSchemas,
  schemaList,
  relAdd,
  dispatch,
}) => {
  const columns = tableSchema.columns.sort(ordinalColSort);

  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
  }));

  relAdd.rSchema = relAdd.rSchema || tableSchema.table_schema;

  const refTables = {};
  allSchemas.forEach(ts => {
    if (ts.table_schema === relAdd.rSchema) {
      refTables[ts.table_name] = ts.columns.map(c => c.column_name);
    }
  });

  const orderedSchemaList = schemaList.map(s => s.schema_name).sort();

  const resetManualRel = () => {
    dispatch(resetManualRelationshipForm());
  };

  const saveFk = toggleEditor => {
    dispatch(addRelViewMigrate(tableSchema, toggleEditor));
  };

  const expandedContent = () => (
    <ManualRelationshipSelector
      tableSchema={tableSchema}
      relAdd={relAdd}
      schemaList={orderedSchemaList}
      refTables={refTables}
      orderedColumns={orderedColumns}
      dispatch={dispatch}
    />
  );

  return (
    <div key="add_manual_relationship">
      <ExpandableEditor
        editorExpanded={expandedContent}
        expandedLabel={null}
        expandButtonText="+ Add a relationship manually"
        collapseCallback={resetManualRel}
        saveFunc={saveFk}
        service="create"
        property="manual-rel"
      />
    </div>
  );
};

export default AddManualRelationship;
