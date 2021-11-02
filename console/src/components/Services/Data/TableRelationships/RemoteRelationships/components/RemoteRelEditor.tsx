import React from 'react';
import { ThunkAction } from 'redux-thunk';
import {
  RemoteRelationship,
  TreeArgElement,
  TreeFieldElement,
  ArgValueKind,
} from '../utils';
import {
  Action as RemoteRelAction,
  setName,
  setRemoteSchema,
  setArgValue,
  setArgValueKind,
  toggleArg,
  toggleField,
} from '../state';

import styles from '../SchemaExplorer.scss';
import {
  RelName as RelNameTooltip,
  RemoteSchema as RemoteSchemaTooltip,
  Configuration as ConfigTooltip,
} from '../Tooltips';
import Explorer from './Explorer';
import { ReduxState, ReduxAction } from '../../../../../../types';
import { Table } from '../../../../../../dataSources/types';
import { PGFunction } from '../../../../../../dataSources/services/postgresql/types';
import {
  getComputedFieldFunction,
  getGroupedTableComputedFields,
} from '../../../../../../dataSources/services/postgresql';

type Props = {
  table: Table;
  remoteSchemas: string[];
  isLast: boolean;
  state: RemoteRelationship;
  dispatch: React.Dispatch<RemoteRelAction>;
  allFunctions: PGFunction[];
  reduxDispatch: ThunkAction<void, ReduxState, unknown, ReduxAction>;
};

const RemoteRelEditor: React.FC<Props> = ({
  table,
  isLast,
  remoteSchemas,
  state,
  allFunctions,
  dispatch,
  reduxDispatch,
}) => {
  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.persist();
    dispatch(setName(e.target.value));
  };

  const handleRemoteSchemaChange = (
    e: React.ChangeEvent<HTMLSelectElement>
  ) => {
    e.persist();
    dispatch(setRemoteSchema(e.target.value));
  };

  const handleFieldToggle = (field: TreeFieldElement) =>
    dispatch(toggleField(field));

  const handleArgToggle = (arg: TreeArgElement) => dispatch(toggleArg(arg));

  const handleArgValueKindChange = (
    arg: TreeArgElement,
    type: ArgValueKind
  ) => {
    dispatch(setArgValueKind(arg, type));
  };

  const handleArgValueChange = (arg: TreeArgElement, value: string) => {
    dispatch(setArgValue(arg, value));
  };

  const tableColumns: string[] = table.columns.map(
    (c: { column_name: string }) => {
      return c.column_name;
    }
  );

  const computedFields = getGroupedTableComputedFields(table, allFunctions);
  const scalarComputedFields = computedFields.scalar.filter(sc => {
    const cFn = getComputedFieldFunction(sc, allFunctions)?.input_arg_types;
    // Only the computed fields that do not require extra arguments other than the table row
    // are currenlty supported by the server https://github.com/hasura/graphql-engine/issues/7336
    return cFn?.length === 1 && cFn[0].name === table.table_name;
  });

  const computedFieldsNames = scalarComputedFields.map(
    f => f.computed_field_name
  );

  return (
    <React.Fragment>
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Name</b>
            </div>
            <div>
              <RelNameTooltip tableName={table.table_name} />
            </div>
          </div>
          <div>
            <input
              type="text"
              className={`form-control ${styles.wd300Px}`}
              placeholder="name"
              value={state.name}
              onChange={handleNameChange}
              disabled={!isLast}
              title={!isLast ? 'Name cannot be changed' : undefined}
            />
          </div>
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex} ${styles.add_mar_right_small}`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Remote Schema:</b>
            </div>
            <div>
              <RemoteSchemaTooltip tableName={table.table_name} />
            </div>
          </div>
          <div>
            <select
              className={`form-control ${styles.wd300Px}`}
              value={state.remoteSchema}
              onChange={handleRemoteSchemaChange}
            >
              <option key="placeholder" value="">
                {' '}
                -- remote schema --
              </option>
              {remoteSchemas.map(s => {
                return (
                  <option key={s} value={s}>
                    {s}
                  </option>
                );
              })}
            </select>
          </div>
        </div>
        <div>
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex} ${styles.add_mar_right_small}`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Configuration:</b>
            </div>
            <div>
              <ConfigTooltip />
            </div>
          </div>
          <Explorer
            relationship={state}
            toggleArg={handleArgToggle}
            toggleField={handleFieldToggle}
            handleArgValueKindChange={handleArgValueKindChange}
            handleArgValueChange={handleArgValueChange}
            remoteSchemaName={state.remoteSchema}
            columns={{
              columns: tableColumns,
              computedFields: computedFieldsNames,
            }}
            reduxDispatch={reduxDispatch}
          />
        </div>
      </div>
    </React.Fragment>
  );
};

export default RemoteRelEditor;
