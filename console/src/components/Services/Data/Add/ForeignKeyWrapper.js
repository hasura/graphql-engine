import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

const ForeignKeyWrapper = ({
  foreignKeys,
  tableName,
  allSchemas,
  columns,
  dispatch,
  styles,
  setForeignKeys,
}) => {
  const orderedColumns = columns
    .filter(c => Boolean(c.name))
    .map((c, i) => ({
      name: c.name,
      type: c.type,
      index: i,
    }));
  const numOfFks = foreignKeys.length;

  const refTables = {};
  allSchemas.forEach(tableSchema => {
    refTables[tableSchema.table_name] = tableSchema.columns.map(
      c => c.column_name
    );
  });

  // TODO check out match full
  return foreignKeys.map((fk, i) => {
    const { colMappings, refTableName, onUpdate, onDelete } = fk;
    const expandButtonText =
      fk.refTableName === '' ? 'Add a foreign key' : 'Edit';
    const filteredColMap = {};
    colMappings
      .filter(colMap => colMap.column !== '' && colMap.refColumn !== '')
      .forEach(
        colMap =>
          (filteredColMap[orderedColumns[colMap.column].name] =
            colMap.refColumn)
      );
    const fkConfig =
      Object.keys(filteredColMap).length !== 0
        ? `( ${Object.keys(filteredColMap).join(', ')} ) → ${refTableName ||
            ''} ( ${Object.values(filteredColMap).join(', ')} )`
        : '';
    const collapsedLabel = () => (
      <div>
        <div className="container-fluid">
          <div className="row">
            <h5 className={styles.padd_bottom}>
              {fkConfig ? <i> {fkConfig} </i> : null}
              &nbsp;
            </h5>
          </div>
        </div>
      </div>
    );
    const expandedContent = () => {
      const configuration = () => {
        return (
          <div>
            <div className={'container-fluid'}>
              <div className="row">
                <h5 className={styles.padd_bottom}>
                  {fkConfig && (
                    <div>
                      Foreign Key: <i>{fkConfig}</i>
                    </div>
                  )}
                  &nbsp;
                </h5>
              </div>
            </div>
          </div>
        );
      };
      const dispatchSetRefTable = event => {
        const newFks = [...foreignKeys];
        newFks[i].refTableName = event.target.value;
        if (i + 1 === numOfFks) {
          newFks.push({
            refTableName: '',
            colMappings: [
              {
                column: '',
                refColumn: '',
              },
            ],
            onUpdate: '',
            onDelete: '',
          });
        }
        dispatch(setForeignKeys(newFks));
      };
      const refTableSelect = () => (
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>Reference Table:</div>
          <select
            value={refTableName || ''}
            className={`${styles.select} ${styles.sample} form-control ${
              styles.add_pad_left
            }`}
            data-test={`foreign-key-ref-table-${i}`}
            data-test={`foreign-key-ref-table-${i}`}
            onChange={dispatchSetRefTable}
          >
            {// default unselected option
              refTableName === '' && (
                <option value={''} disabled>
                  {'reference table'}
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
        const numColMappings = colMappings.length;
        return (
          <div>
            <div className={`row ${styles.add_mar_bottom_mid}`}>
              <div className={`col-sm-4 ${styles.add_mar_right}`}>from: </div>
              <div className={`col-sm-4 ${styles.add_mar_right}`}>to: </div>
            </div>
            {colMappings.map((colMap, _i) => {
              const lc = colMap.column;
              const rc = colMap.refColumn;
              const dispatchSetCols = (key, value) => {
                const newFks = [...foreignKeys];
                newFks[i].colMappings[_i][key] = value;
                if (
                  newFks[i].colMappings[_i].column &&
                  newFks[i].colMappings[_i].refColumn
                ) {
                  newFks[i].colMappings.push({ column: '', refColumn: '' });
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
                newFks[i].colMappings = newColMapping;
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
                  key={`fk-col-${i}-${_i}`}
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
                          {' '}
                          column{' '}
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
                          {' '}
                          ref_column{' '}
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

      return (
        <div className="form-group">
          {configuration()}
          {refTableSelect()}
          {columnSelect()}
        </div>
      );
    };

    // TODO handle ongoing request

    return (
      <div key={i}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          property={'add-fks'}
          ongoingRequest={'oola'}
          service="modify-table"
          collapsedLabel={collapsedLabel}
          expandButtonText={expandButtonText}
        />
      </div>
    );
  });
};

export default ForeignKeyWrapper;
