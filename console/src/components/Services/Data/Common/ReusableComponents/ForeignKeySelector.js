import React from 'react';
import { getForeignKeyConfig } from './utils';

const violiationActions = [
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
];

const ForeignKeySelector = ({
  refTables,
  foreignKey,
  index,
  foreignKeys,
  orderedColumns,
  dispatch,
  styles,
  setForeignKeys,
  service
}) => {
  const { refTableName, colMappings, onUpdate, onDelete } = foreignKey;
  const numOfFks = foreignKeys.length;
  const numColMappings = colMappings.length;
  // generate FK config
  const fkConfig =  numColMappings <= 1 ? '' : getForeignKeyConfig(foreignKey, orderedColumns);

  // html for ref table dropdown
  const refTableSelect = () => {
    // dispatch action for setting reference table
    const dispatchSetRefTable = event => {
      const newFks = [...foreignKeys];
      if (newFks[index].refTableName !== event.target.value) {
        newFks[index].colMappings = [{column: '', refColumn: ''}]
      }
      newFks[index].refTableName = event.target.value;
      if (index + 1 === numOfFks && service === 'add-table') {
        newFks.push({
          refTableName: '',
          colMappings: [
            {
              column: '',
              refColumn: '',
            },
          ],
          onUpdate: 'restrict',
          onDelete: 'restrict',
        });
      }
      dispatch(setForeignKeys(newFks));
    };

    return (
      <div className={`${refTableName ? styles.add_mar_bottom : ''}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={refTableName || ''}
          className={`${styles.select} ${styles.sample} form-control ${
            styles.add_pad_left
          }`}
          data-test={`foreign-key-ref-table-${index}`}
          data-test={`foreign-key-ref-table-${index}`}
          onChange={dispatchSetRefTable}
        >
          {// default unselected option
            refTableName === '' && (
              <option value={''} disabled>
                {'-- reference table --'}
              </option>
            )}
          {// all reference table options
            Object.keys(refTables).map((rt, j) => (
              <option key={j} value={rt}>
                {rt}
              </option>
            ))}
        </select>
      </div>
    )
  };

  // html for column mapping dropdowns
  const columnSelect = () => {
    // Do not allow selecting columns if ref table hasn't been selected
    if (!refTableName) {
      return null;
    }

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
        {colMappings.map((colMap, _i) => {
          // from column
          const lc = colMap.column;

          // to column
          const rc = colMap.refColumn;

          // dispatch action for setting column config
          const dispatchSetCols = (key, value) => {
            const newFks = [...foreignKeys];
            newFks[index].colMappings[_i][key] = value;
            if (
              newFks[index].colMappings[numColMappings - 1].column &&
              newFks[index].colMappings[numColMappings - 1].refColumn
            ) {
              newFks[index].colMappings.push({ column: '', refColumn: '' });
            }
            dispatch(setForeignKeys(newFks));
          };

          // dispatch action for setting the "from" column
          const dispatchSetLcol = event => {
            dispatchSetCols('column', event.target.value);
          };

          // dispatch action for setting the "to" column
          const dispatchSetRcol = event => {
            dispatchSetCols('refColumn', event.target.value);
          };

          // dispatch action for removing a pair from column mapping
          const dispatchRemoveCol = () => {
            const newFks = [...foreignKeys];
            const newColMapping = [
              ...colMappings.slice(0, _i),
              ...colMappings.slice(_i + 1),
            ];
            newFks[index].colMappings = newColMapping;
            dispatch(setForeignKeys(newFks));
          };

          // show remove icon for all column pairs except last
          let removeIcon;
          if (_i + 1 === numColMappings) {
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
              key={`fk-col-${index}-${_i}`}
            >
              <div className={`col-sm-4 ${styles.add_mar_right}`}>
                <select
                  className={'form-control'}
                  value={lc}
                  onChange={dispatchSetLcol}
                  data-test={`foreign-key-lcol-${_i}`}
                  disabled={!refTableName}
                >
                  {lc === '' && (
                    <option value="" disabled>
                      {'-- column --'}
                    </option>
                  )}
                  {orderedColumns.map(oc => {
                    return (
                      <option key={oc.name} value={oc.index}>
                        {oc.name}
                      </option>
                    );
                  })}
                </select>
              </div>
              <div className={'col-sm-4'}>
                <select
                  className={'form-control'}
                  value={rc}
                  onChange={dispatchSetRcol}
                  disabled={!refTableName}
                  data-test={`foreign-key-rcol-${_i}`}
                >
                  {rc === '' && (
                    <option value="" disabled>
                      {'-- ref_column --'}
                    </option>
                  )}
                  {refTables[refTableName] &&
                    refTables[refTableName].map(rcOpt => {
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


  const onViolation = () => {
    // Do not allow selecting on violation conditions if no column mapping is selected
    if (numColMappings <= 1) {
      return null;
    }

    // Generate radios for violation actions
    const radios = action => {
      const selected = foreignKey[action];
      return (
        <div className={'row'}>
          {violiationActions.map(va => {
            const onCheck = () => {
              const newFks = [...foreignKeys];
              newFks[index][action] = va;
              dispatch(setForeignKeys(newFks));
            };
            return (
              <div className={`col-sm-2 ${styles.display_flex}`}>
                <input
                  type="radio"
                  checked={selected === va}
                  onChange={onCheck}
                  className={styles.add_mar_right_small}
                />
                <div>{va.toLowerCase()}</div>
              </div>
            );
          })}
        </div>
      );
    };

    return (
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>On Update Violation:</b>
          </div>
          {radios('onUpdate')}
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>On Delete Violation:</b>
          </div>
          {radios('onDelete')}
        </div>
      </div>
    );
  };

  return (
    <div className="form-group">
      {refTableSelect()}
      {columnSelect()}
      {onViolation()}
    </div>
  );
};

export default ForeignKeySelector;
