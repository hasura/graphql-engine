import React from 'react';
import { getSupportedDrivers } from '../../../../../dataSources';
import styles from '../../../../Common/TableCommon/Table.module.scss';
import {
  relSetDriver,
  relSetName,
  relSetSource,
  relSetTable,
  relSetType,
  relSetColumns,
} from './state';
import { useTableColumns } from '../../../../../features/SqlQueries';
import { getColumnNameArrayFromHookData } from './utils';
import {
  MetadataSelector,
  useMetadata,
} from '../../../../../features/MetadataAPI';
import { FaTimes } from 'react-icons/fa';

const ColumnSelect = ({ orderedColumns, state, dispatch }) => {
  const selectTitle = !state.relTable.name
    ? 'Please select the reference table'
    : undefined;
  const query = useTableColumns(state.relSource, state.relTable);

  const dispatchSetCols = (key, value, index) => {
    const relCols = Object.assign([], state.relColumns);
    relCols[index][key] = value;
    if (
      relCols[state.relColumns.length - 1].column &&
      relCols[state.relColumns.length - 1].refColumn
    ) {
      relCols.push({ column: '', refColumn: '' });
    }
    dispatch(relSetColumns(relCols));
  };

  const dispatchRemoveCol = index => {
    const newColMapping = [
      ...state.relColumns.slice(0, index),
      ...state.relColumns.slice(index + 1),
    ];
    dispatch(relSetColumns(newColMapping));
  };

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
      {state.relColumns.map((colMap, index) => (
        <div
          className={`row ${styles.add_mar_bottom_mid} ${styles.display_flex}`}
          key={`fk-col-${index}`}
        >
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <select
              className={`form-control ${styles.select} ${styles.wd100Percent}`}
              value={colMap.column}
              onChange={e => {
                dispatchSetCols('column', e.target.value, index);
              }}
              data-test={`manual-relationship-lcol-${index}`}
              disabled={!state.relType || !state.relName}
              title={selectTitle}
            >
              {colMap.column === '' && (
                <option value="" disabled>
                  {'-- column --'}
                </option>
              )}
              {orderedColumns.map(oc => (
                <option key={oc.name} value={oc.name}>
                  {oc.name}
                </option>
              ))}
            </select>
          </div>
          <div className={'col-sm-4'}>
            <select
              className={`form-control ${styles.select} ${styles.wd100Percent}`}
              value={colMap.refColumn}
              onChange={e => {
                dispatchSetCols('refColumn', e.target.value, index);
              }}
              disabled={!state.relTable.name}
              title={selectTitle}
              data-test={`manual-relationship-rcol-${index}`}
            >
              {colMap.refColumn === '' && (
                <option value="" disabled>
                  {'-- ref_column --'}
                </option>
              )}
              {query.isSuccess
                ? getColumnNameArrayFromHookData(query.data).map(rcOpt => (
                    <option key={rcOpt} value={rcOpt}>
                      {rcOpt}
                    </option>
                  ))
                : null}
            </select>
          </div>
          <div>
            {index + 1 !== state.relColumns.length ? (
              <FaTimes
                className={`${styles.fontAwosomeClose} text-lg`}
                onClick={() => {
                  dispatchRemoveCol(index);
                }}
              />
            ) : null}
          </div>
        </div>
      ))}
    </div>
  );
};

const ManualRelationshipSelector = ({
  orderedColumns,
  state,
  dispatch,
  isNew,
  currentSource,
}) => {
  const refTables = {};
  const { data: source } = useMetadata(
    MetadataSelector.getDataSourceMetadata(state.relSource)
  );
  const { data: driversList } = useMetadata(MetadataSelector.getAllDriversList);

  if (source) {
    (source.tables ?? []).forEach(x => {
      const { schema, dataset, name } = x.table;
      refTables[name] = schema ?? dataset;
    });
  }

  const dispatchSetRelType = event => {
    dispatch(relSetType(event.target.value));
  };

  const dispatchSetRelName = event => {
    dispatch(relSetName(event.target.value));
  };

  const dispatchSetRefSource = event => {
    dispatch(relSetSource(event.target.value));
    dispatch(
      relSetTable({
        name: '',
        schema: '',
      })
    );
    dispatch(relSetColumns([{ column: '', refColumn: '' }]));
  };

  const dispatchSetRefTable = event => {
    dispatch(relSetDriver(source.kind));
    dispatch(
      relSetTable({
        name: event.target.value,
        schema: refTables[event.target.value],
      })
    );
    dispatch(relSetColumns([{ column: '', refColumn: '' }]));
  };

  return (
    <div className="form-group">
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Type:</b>
        </div>
        <select
          value={state.relType || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-type'}
          onChange={dispatchSetRelType}
        >
          {state.relType === '' && (
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

      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Name:</b>
        </div>
        <input
          onChange={dispatchSetRelName}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          value={state.relName}
          placeholder="Enter relationship name"
          disabled={!state.relType || !isNew}
          data-test="rel-name"
        />
      </div>

      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Source:</b>
        </div>
        <select
          value={state.relSource || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-schema'}
          onChange={dispatchSetRefSource}
          disabled={!state.relType || !state.relName}
        >
          {
            // default unselected option
            state.relSource === '' && (
              <option value={''} disabled>
                {'-- reference source --'}
              </option>
            )
          }
          {
            // all reference source options
            driversList &&
              driversList
                .filter(
                  s =>
                    currentSource !== s.source &&
                    getSupportedDrivers(
                      'tables.relationships.remoteDbRelationships.referenceSource'
                    ).includes(s.kind)
                )
                .map(s => (
                  <option key={s.source} value={s.source}>
                    {s.source}
                  </option>
                ))
          }
        </select>
      </div>

      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={state.relTable.name || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-table'}
          onChange={dispatchSetRefTable}
          disabled={!state.relType || !state.relName}
        >
          {state.relTable.name === '' && (
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

      <ColumnSelect
        orderedColumns={orderedColumns}
        state={state}
        dispatch={dispatch}
      />
    </div>
  );
};

export default ManualRelationshipSelector;
