import React from 'react';

import styles from '../../../../Common/TableCommon/Table.scss';
import { fkViolationOnUpdate, fkViolationOnDelete } from '../TooltipMessages';
import { updateSchemaInfo } from '../../DataActions';
import { Icon, ToolTip, Text, Flex, Box } from '../../../../UIKit/atoms';

const violiationActions = [
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
];

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
      <Box mb="20px">
        <Text fontWeight="bold" mb="10px">
          Reference Schema:
        </Text>
        <select
          value={refSchemaName || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={`foreign-key-ref-schema-${index}`}
          onChange={dispatchSetRefSchema}
        >
          {getRefSchemaOptions()}
        </select>
      </Box>
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
          onUpdate: 'restrict',
          onDelete: 'restrict',
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
      <Box mb="20px">
        <Text fontWeight="bold" mb="10px">
          Reference Table:
        </Text>
        <select
          value={refTableName || ''}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={`foreign-key-ref-table-${index}`}
          onChange={dispatchSetRefTable}
          disabled={!refSchemaName}
        >
          {getRefTableOptions()}
        </select>
      </Box>
    );
  };

  // html for column mapping dropdowns
  const columnSelect = () => {
    // Do not allow selecting columns if ref table hasn't been selected
    const selectTitle = !refTableName
      ? 'Please select the reference table'
      : undefined;
    return (
      <Box mb="20px">
        <Box mb="10px" className="row">
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>From:</b>
          </div>
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>To:</b>
          </div>
        </Box>
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
              <Icon
                type="close"
                onClick={dispatchRemoveCol}
                ml="10px"
                size={15}
                pointer
              />
            );
          }

          return (
            <Flex mb="20px" key={`fk-col-${index}-${_i}`}>
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
            </Flex>
          );
        })}
      </Box>
    );
  };

  const onViolation = () => {
    // Do not allow selecting on violation conditions if no column mapping is selected
    const disabled = numColMappings <= 1;
    const inputTitle = disabled
      ? 'Please select the reference table and the column configuration'
      : undefined;
    // Generate radios for violation actions
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
              <Flex className="col-sm-2" key={`${action}_${va}`}>
                <input
                  type="radio"
                  checked={selected === va}
                  onChange={onCheck}
                  data-test={`foreign-key-${index}-${action}-${va}`}
                  className={styles.add_mar_right_small}
                  title={inputTitle}
                  disabled={disabled}
                />
                <div>{va.toLowerCase()}</div>
              </Flex>
            );
          })}
        </div>
      );
    };

    return (
      <>
        <Box mb="20px">
          <Text fontWeight="bold" mb="10px">
            On Update Violation:
            <ToolTip message={fkViolationOnUpdate} ml="sm" />
          </Text>
          {radios('onUpdate')}
        </Box>
        <Box mb="20px">
          <Text fontWeight="bold" mb="10px">
            On Delete Violation:
            <ToolTip message={fkViolationOnDelete} ml="sm" />
          </Text>
          {radios('onDelete')}
        </Box>
      </>
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
