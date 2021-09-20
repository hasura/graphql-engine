import React from 'react';
import styles from '../../../../Common/TableCommon/Table.scss';
import { fkViolationOnUpdate, fkViolationOnDelete } from '../TooltipMessages';
import { updateSchemaInfo } from '../../DataActions';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import { dataSource } from '../../../../../dataSources';

const ForeignKeySelector = ({
  foreignKey,
  index,
  foreignKeys,
  orderedColumns,
  dispatch,
  setForeignKeys,
  service,
  schemaList,
  refTables,
}) => {
  const { refTableName, colMappings, refSchemaName } = foreignKey;
  const numOfFks = foreignKeys.length;
  const numColMappings = colMappings.length;
  const refSchemaSelect = () => {
    const dispatchSetRefSchema = event => {
      const newFks = JSON.parse(JSON.stringify(foreignKeys));
      if (newFks[index].refSchemaName !== event.target.value) {
        newFks[index].refTableName = '';
        newFks[index].colMappings = [{ column: '', refColumn: '' }];
      }
      newFks[index].refSchemaName = event.target.value;
      dispatch(setForeignKeys(newFks));
      dispatch(updateSchemaInfo({ schemas: [event.target.value] }));
    };

    const getRefSchemaOptions = () => {
      const options = [];

      options.push(
        <option key={-1} value={''} disabled>
          {'-- reference schema --'}
        </option>
      );

      schemaList.forEach((rs, j) => {
        options.push(
          <option key={j} value={rs}>
            {rs}
          </option>
        );
      });

      return options;
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Schema:</b>
        </div>
        <select
          value={refSchemaName || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={`foreign-key-ref-schema-${index}`}
          onChange={dispatchSetRefSchema}
        >
          {getRefSchemaOptions()}
        </select>
      </div>
    );
  };

  // html for ref table dropdown
  const refTableSelect = () => {
    // dispatch action for setting reference table
    const dispatchSetRefTable = event => {
      const newFks = JSON.parse(JSON.stringify(foreignKeys));
      if (newFks[index].refTableName !== event.target.value) {
        newFks[index].colMappings = [{ column: '', refColumn: '' }];
      }
      newFks[index].refTableName = event.target.value;
      if (service === 'add-table' && index + 1 === numOfFks) {
        newFks.push({
          refSchemaName: '',
          refTableName: '',
          colMappings: [
            {
              column: '',
              refColumn: '',
            },
          ],
          onUpdate: dataSource.violationActions[0],
          onDelete: dataSource.violationActions[0],
        });
      }
      dispatch(setForeignKeys(newFks));
    };

    const getRefTableOptions = () => {
      const options = [];

      options.push(
        <option key={-1} value={''} disabled>
          {'-- reference table --'}
        </option>
      );

      // all reference table options
      Object.keys(refTables)
        .sort()
        .forEach((rt, i) => {
          options.push(
            <option key={i} value={rt}>
              {rt}
            </option>
          );
        });

      return options;
    };

    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={refTableName || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={`foreign-key-ref-table-${index}`}
          onChange={dispatchSetRefTable}
          disabled={!refSchemaName}
        >
          {getRefTableOptions()}
        </select>
      </div>
    );
  };

  // html for column mapping dropdowns
  const columnSelect = () => {
    // Do not allow selecting columns if ref table hasn't been selected
    const selectTitle = !refTableName
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
        {colMappings.map((colMap, _i) => {
          // from column
          const lc = colMap.column;

          // to column
          const rc = colMap.refColumn;

          // dispatch action for setting column config
          const dispatchSetCols = (key, value) => {
            const newFks = JSON.parse(JSON.stringify(foreignKeys));
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
            const newFks = JSON.parse(JSON.stringify(foreignKeys));
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
              className={`row ${styles.add_mar_bottom_mid} ${styles.display_flex}`}
              key={`fk-col-${index}-${_i}`}
            >
              <div className={`col-sm-4 ${styles.add_mar_right}`}>
                <select
                  className={`form-control ${styles.select} ${styles.wd100Percent}`}
                  value={lc}
                  onChange={dispatchSetLcol}
                  data-test={`foreign-key-${index}-lcol-${_i}`}
                  disabled={!refTableName}
                  title={selectTitle}
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
                  className={`form-control ${styles.select} ${styles.wd100Percent}`}
                  value={rc}
                  onChange={dispatchSetRcol}
                  disabled={!refTableName}
                  title={selectTitle}
                  data-test={`foreign-key-${index}-rcol-${_i}`}
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
    const disabled = numColMappings <= 1;
    const inputTitle = disabled
      ? 'Please select the reference table and the column configuration'
      : undefined;
    // Generate radios for violation actions
    const violiationActions = dataSource.violationActions;
    const radios = action => {
      const selected = foreignKey[action];
      return (
        <div className={'row'}>
          {violiationActions.map(va => {
            const onCheck = () => {
              const newFks = JSON.parse(JSON.stringify(foreignKeys));
              newFks[index][action] = va;
              dispatch(setForeignKeys(newFks));
            };
            return (
              <div
                className={`col-sm-2 ${styles.display_flex}`}
                key={`${action}_${va}`}
              >
                <input
                  type="radio"
                  checked={selected === va}
                  onChange={onCheck}
                  data-test={`foreign-key-${index}-${action}-${va}`}
                  className={`legacy-input-fix ${styles.add_mar_right_small}`}
                  title={inputTitle}
                  disabled={disabled}
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
            <b>On Update Violation:</b>&nbsp; &nbsp;
            <ToolTip message={fkViolationOnUpdate} />
          </div>
          {radios('onUpdate')}
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>On Delete Violation:</b>&nbsp; &nbsp;
            <ToolTip message={fkViolationOnDelete} />
          </div>
          {radios('onDelete')}
        </div>
      </div>
    );
  };

  return (
    <div className="form-group">
      {refSchemaSelect()}
      {refTableSelect()}
      {columnSelect()}
      {onViolation()}
    </div>
  );
};

export default ForeignKeySelector;
