import React from 'react';
import { fkViolationOnUpdate, fkViolationOnDelete } from '../TooltipMessages';
import { updateSchemaInfo } from '../../DataActions';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import { dataSource } from '../../../../../dataSources';
import { FaTimes } from 'react-icons/fa';
import { inputStyles } from '../../constants';

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
      <div className="mb-md sm:w-6/12">
        <h4 className="flex items-center text-gray-600 font-semibold mb-sm">
          Reference Schema:
        </h4>
        <select
          value={refSchemaName || ''}
          className={inputStyles}
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
      <div className="mb-md sm:w-6/12">
        <h4 className="flex items-center text-gray-600 font-semibold mb-sm">
          Reference Table:
        </h4>
        <select
          value={refTableName || ''}
          className={inputStyles}
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
      <div className="mb-md space-y-md">
        <div className="grid gap-sm grid-cols-1 sm:grid-cols-3">
          <div>
            <span className="flex items-center text-gray-600 font-semibold">
              From:
            </span>
          </div>
          <div>
            <span className="flex items-center text-gray-600 font-semibold">
              To:
            </span>
          </div>
          <div />
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
              <FaTimes
                className="w-4 cursor-pointer"
                onClick={dispatchRemoveCol}
              />
            );
          }

          return (
            <div
              className="grid gap-sm grid-cols-1 sm:grid-cols-3"
              key={`fk-col-${index}-${_i}`}
            >
              <div>
                <select
                  className={`form-control`}
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
              <div>
                <select
                  className={`form-control`}
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
              <div className="flex items-center">{removeIcon}</div>
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
        <div>
          {violiationActions.map(va => {
            const onCheck = () => {
              const newFks = JSON.parse(JSON.stringify(foreignKeys));
              newFks[index][action] = va;
              dispatch(setForeignKeys(newFks));
            };
            return (
              <div className="inline-flex mr-md" key={`${action}_${va}`}>
                <input
                  type="radio"
                  checked={selected === va}
                  onChange={onCheck}
                  data-test={`foreign-key-${index}-${action}-${va}`}
                  className={`legacy-input-fix`}
                  title={inputTitle}
                  disabled={disabled}
                />
                <div className="ml-xs">{va.toLowerCase()}</div>
              </div>
            );
          })}
        </div>
      );
    };

    return (
      <div>
        <div className="mb-md">
          <div className="mb-sm flex items-center text-gray-600 font-semibold mb-sm">
            <span>On Update Violation:</span>
            <ToolTip message={fkViolationOnUpdate} />
          </div>
          {radios('onUpdate')}
        </div>
        <div>
          <div className="flex items-center text-gray-600 font-semibold mb-sm">
            <span>On Delete Violation:</span>
            <ToolTip message={fkViolationOnDelete} />
          </div>
          {radios('onDelete')}
        </div>
      </div>
    );
  };

  return (
    <div>
      {refSchemaSelect()}
      {refTableSelect()}
      {columnSelect()}
      {onViolation()}
    </div>
  );
};

export default ForeignKeySelector;
