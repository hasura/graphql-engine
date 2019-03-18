import React from 'react';
import { getForeignKeyConfig } from './utils';

const violiationActions = [
  'restrict',
  'delete',
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
}) => {
  const { refTableName, colMappings, onUpdate, onDelete } = foreignKey;
  const numOfFks = foreignKeys.length;
  const fkConfig = getForeignKeyConfig(foreignKey, orderedColumns);
  const configuration = () => {
    if (!fkConfig) return null;
    return (
      <div className={`${refTableName ? styles.add_mar_bottom : ''}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Foreign key:</b>
        </div>
        <div>
          <div>
            Constraint: <i>{fkConfig}</i>
          </div>
          <div>
            On update violation: <i>{onUpdate}</i>
          </div>
          <div>
            On delete violation: <i>{onDelete}</i>
          </div>
        </div>
      </div>
    );
  };
  const dispatchSetRefTable = event => {
    const newFks = [...foreignKeys];
    newFks[index].refTableName = event.target.value;
    if (index + 1 === numOfFks) {
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
  const refTableSelect = () => (
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
  );

  const columnSelect = () => {
    if (!refTableName) {
      return null;
    }
    const numColMappings = colMappings.length;
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
          const lc = colMap.column;
          const rc = colMap.refColumn;
          const dispatchSetCols = (key, value) => {
            const newFks = [...foreignKeys];
            newFks[index].colMappings[_i][key] = value;
            if (
              newFks[index].colMappings[_i].column &&
              newFks[index].colMappings[_i].refColumn
            ) {
              newFks[index].colMappings.push({ column: '', refColumn: '' });
            }
            dispatch(setForeignKeys(newFks));
          };
          const dispatchSetLcol = event => {
            dispatchSetCols('column', event.target.value);
          };
          const dispatchSetRcol = event => {
            dispatchSetCols('refColumn', event.target.value);
          };
          const dispatchRemoveCol = () => {
            const newFks = [...foreignKeys];
            const newColMapping = [
              ...colMappings.slice(0, _i),
              ...colMappings.slice(_i + 1),
            ];
            newFks[index].colMappings = newColMapping;
            dispatch(setForeignKeys(newFks));
          };
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
    if (!fkConfig) {
      return null;
    }
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
      {configuration()}
      {refTableSelect()}
      {columnSelect()}
      {onViolation()}
    </div>
  );
};

export default ForeignKeySelector;
