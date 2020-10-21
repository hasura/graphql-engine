import React from 'react';

import { OrderBy } from '../utils/v1QueryUtils';
import { useFilterQuery, TriggerOperation } from './state';
import { Filter, FilterRenderProp } from './types';
import { Nullable } from '../utils/tsUtils';
import styles from './FilterQuery.scss';
import { BaseTable } from '../../../dataSources/types';
import { generateTableDef } from '../../../dataSources';
import { Dispatch } from '../../../types';
import { EventKind } from '../../Services/Events/types';
// import ReloadEnumValuesButton from '../../Services/Data/Common/Components/ReloadEnumValuesButton';
// import Button from '../Button/Button';
// import Where from './Where';
// import Sorts from './Sorts';

interface Props {
  table: BaseTable;
  relationships: Nullable<string[]>; // TODO better
  render: FilterRenderProp;
  presets: {
    filters: Filter[];
    sorts: OrderBy[];
  };
  dispatch: Dispatch;
  triggerOp: TriggerOperation;
  triggerType: EventKind;
  triggerName?: string;
  currentSource?: string; // mainly needed by data triggers
}

/*
 * Where clause and sorts builder
 * Accepts a render prop to render the results of filter/sort query
 */

const FilterQuery: React.FC<Props> = props => {
  const {
    table,
    dispatch,
    presets,
    render,
    relationships,
    triggerName,
    currentSource,
    triggerOp,
    triggerType,
  } = props;

  const { rows, count, runQuery, state, setState } = useFilterQuery(
    generateTableDef(table.table_name, table.table_schema),
    dispatch,
    presets,
    relationships,
    triggerOp,
    triggerType,
    triggerName,
    currentSource
  );

  return (
    <div className={styles.add_mar_top}>
      {/* NOTE: temp. disabled until an API is ready for this purpose */}
      {/* <form
        onSubmit={e => {
          e.preventDefault();
          runQuery();
        }}
        className={styles.add_mar_bottom}
      >
        <div>
          <div
            className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
          >
            <span className={styles.subheading_text}>Filter</span>
            <Where
              filters={state.filters}
              setFilters={setState.filters}
              table={table}
            />
          </div>
          <div
            className={`${styles.queryBox} col-xs-6 ${styles.padd_left_remove}`}
          >
            <b className={styles.subheading_text}>Sort</b>
            <Sorts
              sorts={state.sorts}
              setSorts={setState.sorts}
              table={table}
            />
          </div>
        </div>
        <div className={`${styles.padd_right} ${styles.clear_fix}`}>
          <Button
            type="submit"
            color="yellow"
            size="sm"
            data-test="run-query"
            className={styles.add_mar_right}
          >
            Run query
          </Button>
          {table.is_enum ? (
            <ReloadEnumValuesButton
              dispatch={dispatch}
              tooltipStyle={styles.add_mar_left_mid}
            />
          ) : null}
        </div>
      </form> */}
      {/* TODO: Handle loading state */}
      {render(rows, count, state, setState, runQuery)}
    </div>
  );
};

export default FilterQuery;
